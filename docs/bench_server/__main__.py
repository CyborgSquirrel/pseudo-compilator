import asyncio
import httpx_ws
import json
import random
import textwrap

import httpx
import proto

SERVER_URL = "http://127.0.0.1:8000"


CODE_SNIPPETS = [
    textwrap.dedent("""
        pentru i<-1,10 executa
        \tscrie "ok"
    """),
    textwrap.dedent("""
        cat timp 1=1 executa
        \tscrie "ok"
    """),
    textwrap.dedent("""
        list <- ,
        cat timp 1=1 executa
        \tinsereaza list,lungime(list),1
    """),
]


async def requester(code_snippet: str):
    # print(code_snippet, flush=True)
    content = proto.PostJob(code=code_snippet, language_enable_lists=True)
    content = content.to_json()
    async with httpx.AsyncClient() as client:
        response = await client.post(f"{SERVER_URL}/job", content=content)
        # print(response.content)
        response: proto.JobResponse = proto.JobResponse().from_json(response.content)
        job_id = response.success.job_id
        try:
            async with httpx_ws.aconnect_ws(f"{SERVER_URL}/job/{job_id}/status") as ws:
                while True:
                    event = await ws.receive()
        except httpx_ws.WebSocketDisconnect:
            pass


async def main():
    async with asyncio.TaskGroup() as group:
        for _ in range(100):
            code_snippet = random.choices(CODE_SNIPPETS)[0]
            group.create_task(requester(code_snippet))
            await asyncio.sleep(0.01)


if __name__ == "__main__":
    asyncio.run(main())
