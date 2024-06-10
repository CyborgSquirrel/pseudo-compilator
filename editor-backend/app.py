import asyncio
import codecs
import dataclasses
import itertools
import json
import logging
import os
import pathlib
import shlex
import shutil
import subprocess
import typing

import betterproto
import proto
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

CPU_TIME_LIMIT = 1
MEMORY_LIMIT = 1024*1024*16
SOURCE_LIMIT = 1024*64

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


class JobEndpoint(HTTPEndpoint):
    async def post(self, request: Request):
        body = await request.json()
        parsed_request: proto.PostJob = proto.PostJob().from_dict(body)

        if len(parsed_request.code) > SOURCE_LIMIT:
            response = proto.JobResponse(
                other_error="Eroare: codul sursă depășește dimensiunea maximă."
            )

            return JSONResponse(response.to_dict())

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

            response = proto.JobResponse(
                parser_error=proto.JobResponseParserError(
                    line=error["line"],
                    column=error["column"],
                    message=error["message"],
                )
            )

            return JSONResponse(response.to_dict())
        job.status = "compiled"

        response = proto.JobResponse(
            success=proto.JobResponseSuccess(
                job_id=job_id,
            )
        )
        
        return JSONResponse(response.to_dict())


class JobStatusEndpoint(WebSocketEndpoint):
    encoding = "json"

    @staticmethod
    async def _monitor_process(
        job_id: int,
        websocket: WebSocket,
        process: asyncio.subprocess.Process,
    ):
        decoder = codecs.getincrementaldecoder("utf8")()
        while True:
            data = await process.stdout.read(256)
            final = len(data) == 0
            decoded = decoder.decode(data, final)

            message = proto.JobOutput(
                stdout=proto.JobOutputStdout(
                    text=decoded,
                )
            )
            
            await websocket.send_json(message.to_dict())
            if final:
                break

        await process.communicate()

        if process.returncode != 0:
            logger.info("non-zero return code %s", process.returncode)

        match process.returncode:
            case 0:
                pass
            
            # NOTE: Return code of 139 means that the process was ended with
            # SIGKILL. Since we're running the process using prlimit, we're going to
            # assume that it was killed because it exceeded its allotted limits.
            case 139:
                message = proto.JobOutput(
                    stdout=proto.JobOutputStdout(
                        text="Eroare: programul a depășit resursele de procesor care i-au fost alocate.\n",
                    )
                )
            
                await websocket.send_json(message.to_dict())

            # Return code of 134 _typically_ means segfault. And return code 137
            # means out of memory.
            case item if item in [134, 137]:
                message = proto.JobOutput(
                    stdout=proto.JobOutputStdout(
                        text="Eroare: programul a depășit memoria care i-a fost alocată.\n",
                    )
                )
            
                await websocket.send_json(message.to_dict())

            case other:
                logger.warning("unexpected return code %s", other)

        await websocket.close()
        if (job := jobs.get(job_id)) is not None:
            del jobs[job_id]
            shutil.rmtree(job.state_path)

    async def on_connect(self, websocket: WebSocket):
        job_id = int(websocket.path_params["job_id"])
        job = jobs[job_id]

        process = await asyncio.subprocess.create_subprocess_exec(
            # NOTE: Use script command to run the executable, so that its stdout
            # is line-buffered. Many thanks to this[0] stackexchange post.
            # [0]: https://unix.stackexchange.com/a/61833
            "script",
            "-q",
            "-E", "never",
            "-c", shlex.join([
                # NOTE: Use prlimit command, to limit the CPU time that the
                # executable is allowed to run for.
                "prlimit",
                f"--cpu={CPU_TIME_LIMIT}",
                f"--data={MEMORY_LIMIT}",
                "--",
                str(job.executable_path),
            ]),
            "-e",
            "/dev/null",
            stdout=asyncio.subprocess.PIPE,
            stdin=asyncio.subprocess.PIPE,
        )
        job.status = "running"
        job.process = process

        asyncio.create_task(self._monitor_process(job_id, websocket, process))
        
        await websocket.accept()

    async def on_receive(self, websocket: WebSocket, data):
        job_id = int(websocket.path_params["job_id"])
        job = jobs[job_id]

        message: proto.JobInput = proto.JobInput().from_dict(data)
        type_, message = betterproto.which_one_of(message, "message")
        match type_:
            case "stdin":
                job.process.stdin.write(message.text.encode())
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
