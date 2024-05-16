import asyncio
import codecs
import dataclasses
import itertools
import json
import logging
import os
import pathlib
import shutil
import subprocess
import typing

import pydantic
from starlette.applications import Starlette
from starlette.endpoints import HTTPEndpoint, WebSocketEndpoint
from starlette.middleware import Middleware
from starlette.middleware.cors import CORSMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.routing import Route, WebSocketRoute
from starlette.websockets import WebSocket

logger = logging.getLogger(__name__)
logging.basicConfig(level=os.environ.get("LOG_LEVEL"))

file = pathlib.Path(__file__)

PSEUDO_WORKSPACE_PATH = file.parent.parent / "pseudo"
COMPILER_PATH = PSEUDO_WORKSPACE_PATH / "target" / "debug" / "pseudo-cli"
LIB_PATH = PSEUDO_WORKSPACE_PATH / "target" / "debug" / "libpseudo_sys.so"

# make sure the compiler is built
subprocess.run([
    "cargo", "build",
], cwd=str(PSEUDO_WORKSPACE_PATH))

# state directory logic
STATE_PATH = pathlib.Path("./state")
MARKER_PATH = STATE_PATH / "marker"
MARKER_BYTES = b"this directory is managed by pseudocode editor backend"

marker = STATE_PATH / "marker"
if STATE_PATH.exists():
    if marker.exists() and marker.read_bytes() == MARKER_BYTES:
        shutil.rmtree(STATE_PATH)
    else:
        raise RuntimeError(f"f{STATE_PATH=} already exists, and is missing the marker, therefore can't delete it")

STATE_PATH.mkdir(parents=True)
MARKER_PATH.write_bytes(MARKER_BYTES)


# job stuff
@dataclasses.dataclass
class Job:
    id: int
    status: typing.Literal["created", "compiled", "running"]
    process: typing.Optional[asyncio.subprocess.Process] = None

    @property
    def state_path(self) -> pathlib.Path:
        return STATE_PATH / "job" / str(self.id)

    @property
    def code_path(self) -> pathlib.Path:
        return self.state_path / "code"

    @property
    def executable_path(self) -> pathlib.Path:
        return self.state_path / "executable"


job_ids = itertools.count()
jobs: dict[int, Job] = dict()


class JobResponseSuccess(pydantic.BaseModel):
    type: typing.Literal["success"]
    job_id: int


class JobResponseFailure(pydantic.BaseModel):
    type: typing.Literal["failure"]
    error: typing.Any


class JobResponse(pydantic.BaseModel):
    message: typing.Union[JobResponseSuccess, JobResponseFailure] = pydantic.Field(..., discriminator="type")


class JobEndpoint(HTTPEndpoint):
    class Post(pydantic.BaseModel):
        code: str
        language_enable_lists: bool

    async def post(self, request: Request):
        body = await request.body()
        parsed_request: self.Post = self.Post.parse_raw(body)

        job_id = next(job_ids)
        job = Job(job_id, "created")
        jobs[job_id] = job
        job.state_path.mkdir(parents=True)
        
        # compile the code
        job.code_path.write_text(parsed_request.code)
        process = await asyncio.subprocess.create_subprocess_exec(
            str(COMPILER_PATH),
            "--lib-path", str(LIB_PATH),
            "--executable",
            "--output", "json",
            "--language-enable-lists", str(parsed_request.language_enable_lists).lower(),
            str(job.code_path),
            str(job.executable_path),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.STDOUT,
        )
        (stdout, stderr) = await process.communicate()

        if process.returncode != 0:
            shutil.rmtree(job.state_path)
            del jobs[job.id]

            try:
                error = json.loads(stdout)
            except json.JSONDecodeError:
                error = None
                logger.info("failed to decode compiler error %s", stdout)

            return JSONResponse(dict(message=dict(type="failure", error=error)))
        job.status = "compiled"
        
        return JSONResponse(dict(message=dict(type="success", job_id=job_id)))


class ReceiveStdin(pydantic.BaseModel):
    type: typing.Literal["stdin"]
    stdin: str


class ReceiveStop(pydantic.BaseModel):
    type: typing.Literal["stop"]


class Receive(pydantic.BaseModel):
    message: typing.Union[ReceiveStdin, ReceiveStop] = pydantic.Field(..., discriminator="type")


class JobStatusEndpoint(WebSocketEndpoint):
    encoding = "json"

    class Send(pydantic.BaseModel):
        stdout: str
    
    @staticmethod
    async def _send_stdout(
        job_id: int,
        websocket: WebSocket,
        process: asyncio.subprocess.Process,
    ):
        decoder = codecs.getincrementaldecoder("utf8")()
        while True:
            data = await process.stdout.read(256)
            final = len(data) == 0
            decoded = decoder.decode(data, final)
            await websocket.send_json(dict(JobStatusEndpoint.Send(stdout=decoded)))
            if final:
                break
        await websocket.close()

        job = jobs[job_id]
        del jobs[job_id]
        shutil.rmtree(job.state_path)

    async def on_connect(self, websocket: WebSocket):
        job_id = int(websocket.path_params["job_id"])
        job = jobs[job_id]

        # NOTE: Use script command to run the executable, so that its stdout is
        # line-buffered. Many thanks to this[0] stackexchange post.
        # [0]: https://unix.stackexchange.com/a/61833
        process = await asyncio.subprocess.create_subprocess_exec(
            "script",
            "-q",
            "-c", str(job.executable_path),
            "-E", "never",
            "/dev/null",
            stdout=asyncio.subprocess.PIPE,
            stdin=asyncio.subprocess.PIPE,
        )
        job.status = "running"
        job.process = process

        asyncio.create_task(self._send_stdout(job_id, websocket, process))
        
        await websocket.accept()

    async def on_receive(self, websocket: WebSocket, data):
        job_id = int(websocket.path_params["job_id"])
        job = jobs[job_id]

        parsed_data: Receive = Receive.parse_obj(data)
        logger.info("job %s %s", job_id, parsed_data.message.type)
        match parsed_data.message.type:
            case "stdin":
                job.process.stdin.write(parsed_data.message.stdin.encode())
                await job.process.stdin.drain()
            case "stop":
                job.process.kill()

    async def on_disconnect(self, websocket: WebSocket, data):
        job_id = int(websocket.path_params["job_id"])
        if (job := jobs.get(job_id)) is not None:
            del jobs[job_id]
            shutil.rmtree(job.state_path)


routes = [
    Route("/job", JobEndpoint),
    WebSocketRoute("/job/{job_id:int}/status", JobStatusEndpoint)
]
middleware = [
    Middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_methods=["*"],
        allow_headers=["*"],
    )
]
app = Starlette(routes=routes, middleware=middleware)
