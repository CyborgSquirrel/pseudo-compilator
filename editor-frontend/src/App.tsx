import React, { useState } from 'react';
import './App.css';
import CodeMirror, { Decoration, DecorationSet, EditorView, GutterMarker, ReactCodeMirrorRef, StateEffect, StateField, gutter } from '@uiw/react-codemirror';
import { indentUnit } from '@codemirror/language'
import axios from 'axios';
import CommandLine from './CommandLine';
import { PlayArrow, QuestionMark, Settings, Stop } from '@mui/icons-material';
import SettingsModal from './SettingsModal';
import { JobInput, JobOutput, JobResponse, PostJob } from './proto/main'
import HelpModal from './HelpModal';

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

const iconStyle = {
  width: "50px",
  height: "50px",
};

function App() {
  const instance = React.useMemo(() => {
    return axios.create({
      baseURL: `http://${endpoint}`,
    });
  }, [endpoint]);

  const [jobId, setJobId] = React.useState<null|number>(null);
  const [error, setError] = React.useState<null|JobResponse.IParserError>(null);
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
    if (error !== null && error !== undefined) {
      setValueOutput(error.message!);
    }

    const editor = editorRef.current;
    if (editor !== null && editor.view !== undefined && editor.state !== undefined) {
      editor.view.dispatch({
        effects: [removeLineErrors.of(null)],
      });
      if (error !== null && error !== undefined) {
        const line = editor.view.state.doc.line(error.line!);
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
        const messageObject = JSON.parse(event.data);
        const message = JobOutput.fromObject(messageObject)
        switch (message.message) {
          case "stdout": {
            setValueOutput((valueOutput) => valueOutput + message.stdout!.text);
            break;
          }
        }
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

  const [openSettings, setOpenSettings] = React.useState(false);
  const [openHelp, setOpenHelp] = React.useState(false);

  const [compileLists, setCompileLists] = React.useState(() => {
    const valueRaw = localStorage.getItem("compileLists");
    if (valueRaw === null) return false;
    const value: boolean = JSON.parse(valueRaw);
    return value;
  });
  React.useEffect(() => {
    localStorage.setItem("compileLists", JSON.stringify(compileLists));
  }, [compileLists])

  return (
    <div
      style={{
        height: "100vh",
      }}
    >

    <SettingsModal
      open={openSettings}
      setOpen={setOpenSettings}
      compileLists={compileLists}
      setCompileLists={setCompileLists}
    />

    <HelpModal
      open={openHelp}
      setOpen={setOpenHelp}
    />

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
        <PlayArrow
          onClick={async () => {
            setValueOutput("");
            setJobId(null);
            setError(null);

            const request = PostJob.create({
              code: valueCode,
              languageEnableLists: compileLists,
            });

            const axiosResponse = await instance.post("/job", request.toJSON());
            const response = JobResponse.fromObject(axiosResponse.data);

            switch (response.result) {
              case "success": {
                setJobId(response.success!.jobId!);
                break;
              }
              case "parserError": {
                setError(response.parserError!)
                break;
              }
              case "otherError": {
                setValueOutput(response.otherError! + "\n");
                break;
              }
            }
          }}
          style={iconStyle}
        />
        <Stop
          onClick={async () => {
            if (socket === null) return;

            const message = JobInput.fromObject({
              stop: {},
            })

            socket.send(JSON.stringify(message.toJSON()));
          }}
          style={iconStyle}
        />
        {/* NOTE: Have to wrap some icons in a <div>, because their size doesn't
        match up with the other icons. I use the <div> to shrink them, and make
        sure to center them inside the <div>. */}
        <div
          onClick={() => {
            setOpenSettings(true);
          }}
          style={{
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            ...iconStyle
          }}
        >
          <Settings
            style={{
              width: "80%",
              height: "80%",
            }}
          />
        </div>

        {/* Put help button at the bottom, to reduce clutter. */}

        <div
          style={{
            flexGrow: 1,
          }}
        />

        <div
          onClick={() => {
            setOpenHelp(true);
          }}
          style={{
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            ...iconStyle
          }}
        >
          <QuestionMark
            style={{
              width: "80%",
              height: "80%",
            }}
          />
        </div>
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

                const message = JobInput.create({
                  stdin: {
                    text: `${value}\n`,
                  }
                })
                
                socket.send(JSON.stringify(message));
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
