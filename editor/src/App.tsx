import React, { useState } from 'react';
import './App.css';
import CodeMirror, { Decoration, DecorationSet, EditorView, GutterMarker, ReactCodeMirrorRef, StateEffect, StateField, gutter } from '@uiw/react-codemirror';
import { indentUnit } from '@codemirror/language'
import axios from 'axios';
import { BsPlayFill, BsFillStopFill } from "react-icons/bs";
import CommandLine from './CommandLine';

const endpoint = "localhost:8000";

const removeLineErrors = StateEffect.define();

const addLineError = StateEffect.define<{from: number}>({
  map: ({from}, change) => ({from: change.mapPos(from)})
});

const lineErrorMark = Decoration.line({
  attributes: {
    style:
        "text-decoration-style: wavy;"
      + "text-decoration-line: underline;"
      + "text-decoration-thickness: 1px;"
      + "text-decoration-color: red;"
  },
});

const lineErrorField = StateField.define({
  create() {
    return Decoration.none;
  },
  update(lines, tr) {
    lines = lines.map(tr.changes);
    for (let e of tr.effects) {
      if (e.is(removeLineErrors)) {
        lines = Decoration.none;
      }
      if (e.is(addLineError)) {
        lines = lines.update({add: [lineErrorMark.range(e.value.from)]});
      }
    }
    return lines;
  },
  provide: (f) => EditorView.decorations.from(f),
});

function App() {
  const instance = React.useMemo(() => {
    return axios.create({
      baseURL: `http://${endpoint}`,
    });
  }, [endpoint]);

  const [jobId, setJobId] = React.useState<null|number>(null);
  const [error, setError] = React.useState<null|any>(null);
  const [socket, setSocket] = React.useState<null|WebSocket>(null);
  const running = socket !== null;

  const [valueCode, setValueCode] = React.useState("");
  const onChangeCode = React.useCallback((val: any, viewUpdate: any) => {
    setValueCode(val);
  }, []);

  const editorRef = React.useRef<null|ReactCodeMirrorRef>(null);

  const [commandLineValue, setCommandLineValue] = useState("");
  const commandLineRef = React.useRef<null|HTMLDivElement>(null);

  React.useEffect(() => {
    if (error !== null) {
      setValueOutput(error.message);
    }

    const editor = editorRef.current;
    if (editor !== null && editor.view !== undefined && editor.state !== undefined) {
      editor.view.dispatch({
        effects: [removeLineErrors.of(null)],
      });
      if (error !== null) {
        const line = editor.view.state.doc.line(error.line);
        editor.view.dispatch({
          effects: [addLineError.of({from: line.from})],
        });
      }
    }
  }, [error]);

  const outputRef = React.useRef<null|HTMLDivElement>(null);
  const [valueOutput, setValueOutput] = React.useState("");

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

  React.useEffect(() => {
    const output = outputRef.current;
    if (output === null) return;
    output.scrollTo(0, output.scrollHeight);
  }, [valueOutput]);

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
        }}
      >
        <BsPlayFill
          onClick={async () => {
            setValueOutput("");
            setJobId(null);
            setError(null);

            const response = await instance.post("/job", {code: valueCode});
            const data = response.data;

            switch (data.message.type) {
              case "success": {
                setJobId(data.message.job_id);
                break;
              }
              case "failure": {
                setError(data.message.error)
                break;
              }
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
            flexShrink: 1,
            flexBasis: 0,

            maxHeight: "100%",
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

              ref={editorRef}

              extensions={[
                indentUnit.of("\t"),
                lineErrorField,
              ]}
              indentWithTab={true}

              value={valueCode}
              onChange={onChangeCode}
            />
          </div>
        </div>
        <div
          style={{
            flexGrow: 1,
            flexShrink: 1,
            flexBasis: 0,

            maxHeight: "100%",
            display: "flex",
            flexDirection: "column",
            overflowY: "auto",

            backgroundColor: "#f5f5f5",
          }}
        >
          <div>console {running ? "(running)" : ""}</div>
          <div
            style={{
              paddingLeft: "6px",
              paddingRight: "2px",
              paddingTop: "4px",
              paddingBottom: "4px",
              backgroundColor: "white",
              fontFamily: "monospace",

              display: "flex",
              flexDirection: "column",

              overflowY: "auto",
            }}
            onClick={(_event) => {
              if (commandLineRef.current === null) return;
              commandLineRef.current.focus();
            }}
          >
            <div
              ref={outputRef}
              style={{
                whiteSpace: "break-spaces",
                overflowY: "auto",
              }}
            >{valueOutput}</div>
            <CommandLine
              style={{
                flexGrow: 1,
                flexBasis: "auto",
              }}
              ref={commandLineRef}
              value={commandLineValue}
              setValue={setCommandLineValue}
              onSubmit={(value) => {
                if (socket === null) {
                  return;
                }

                setCommandLineValue("");
                socket.send(JSON.stringify({
                  message: {
                    type: "stdin",
                    stdin: `${value}\n`,
                  }
                }));
              }}
            />
          </div>
        </div>
        </div>
    </div>

    </div>
  );
}

export default App;
