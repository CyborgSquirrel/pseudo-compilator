import React from 'react';
import './App.css';
import CodeMirror from '@uiw/react-codemirror';
import { indentUnit } from '@codemirror/language'
import axios from 'axios';
import { BsPlayFill, BsFillStopFill } from "react-icons/bs";

const endpoint = "localhost:8000";

function App() {
  const instance = axios.create({
    baseURL: `http://${endpoint}`,
  });

  const [jobId, setJobId] = React.useState<null|number>(null);
  const [socket, setSocket] = React.useState<null|WebSocket>(null);
  const running = socket !== null;

  const inputRef = React.useRef<null|HTMLDivElement>(null);

  React.useEffect(() => {
    if (jobId === null && socket !== null) {
      socket.close();
      setSocket(null);
    }

    if (jobId !== null) {
      const socket = new WebSocket(`ws://${endpoint}/job/${jobId}/status`);
      socket.addEventListener("message", (event) => {
        const data = JSON.parse(event.data);
        setValueOutput((valueOutput) => valueOutput + data.stdout);
      });
      socket.addEventListener("close", (_event) => {
        setJobId(null);
      });
      
      setSocket(socket);
    }
  }, [jobId]);

  const [valueCode, setValueCode] = React.useState("");
  const onChangeCode = React.useCallback((val: any, viewUpdate: any) => {
    setValueCode(val);
  }, []);

  const [valueOutput, setValueOutput] = React.useState("");
  const onChangeOutput = React.useCallback((val: any, viewUpdate: any) => {
    setValueOutput(val);
  }, []);
  
  return (
    <div
      style={{
        height: "100vh",
      }}
    >

    <div
      style={{
        height: "100%",
        display: "flex",
        flexDirection: "row",
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          backgroundColor: "#f5f5f5",

          // borderRight: "1px",
          // borderLeft: "0",
          // borderTop: "0",
          // borderBottom: "0",
          // borderStyle: "solid",
          // borderColor: "black",
        }}
      >
        <BsPlayFill
          onClick={async () => {
            setValueOutput("");
            setJobId(null);
            
            const response = await instance.post("/job", {code: valueCode});
            const data = response.data;

            console.log(response);
            if ("job_id" in data) {
              setJobId(data.job_id);
            } else {
              setValueOutput(data.value)
            }
          }}
          style={{
            width: "50px",
            height: "50px",
          }}
        />
        <BsFillStopFill
          onClick={async () => {
            if (socket === null) return;

            socket.send(JSON.stringify({
              message: {
                type: "stop",
              }
            }));
          }}
          style={{
            width: "50px",
            height: "50px",
          }}
        />
      </div>
      <div
        style={{
          height: "100%",
          width: "100%",
          display: "flex",
          flexDirection: "column",
        }}
      >
        <div
          style={{
            flexGrow: 1,

            display: "flex",
            flexDirection: "column",

            backgroundColor: "#f5f5f5",
          }}
        >
          <div>code</div>
          <div
            style={{
              height: "100%",
              width: "100%",
              position: "relative",
            }}
          >
            <CodeMirror
              // NOTE: Need to have height="100%" both in style and in the tag for
              // some reason...
              style={{
                height: "100%",
                width: "100%",
                position: "absolute",
              }}
              height="100%"

              value={valueCode}
              extensions={[
                indentUnit.of("\t"),
              ]}
              indentWithTab={true}
              onChange={onChangeCode}
            />
          </div>
        </div>
        <div
          style={{
            flexGrow: 1,

            display: "flex",
            flexDirection: "column",

            backgroundColor: "#f5f5f5",
          }}
        >
          <div>output {running ? "(running)" : ""}</div>
          <div
            style={{
              height: "100%",
              width: "100%",
              position: "relative",
            }}
          >
            <CodeMirror
              style={{
                height: "100%",
                width: "100%",
                position: "absolute",
              }}
              height="100%"

              value={valueOutput}
              onChange={onChangeOutput}
              editable={false}
            />
          </div>
        </div>
        <div
          style={{
            width: "100%",
            border: "0",
          }}
        >
          <div
            contentEditable={true}
            ref={inputRef}
            style={{
            }}
            onKeyDown={(event) => {
              if (event.key === "Enter") {
                event.preventDefault();
                if (socket === null) {
                  console.warn("socket is null when it shouldn't be");
                  return;
                }
                if (inputRef.current === null) {
                  return;
                }

                const text = inputRef.current.innerText;
                socket.send(JSON.stringify({
                  message: {
                    type: "stdin",
                    stdin: `${text}\n`,
                  }
                }));
              }
            }}
          />
        </div>
      </div>
    </div>

    </div>
  );
}

export default App;
