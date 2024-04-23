import React, { useState } from 'react';
import './App.css';
import CodeMirror, { Decoration, DecorationSet, EditorView, GutterMarker, ReactCodeMirrorRef, StateEffect, StateField, gutter } from '@uiw/react-codemirror';
import { indentUnit } from '@codemirror/language'
import axios from 'axios';
import { BsPlayFill, BsFillStopFill } from "react-icons/bs";

const endpoint = "localhost:8000";

const emptyMarker = new class extends GutterMarker {
  toDOM() {
    const node = document.createElement("span");
    node.innerText = "●";
    node.style.color = "red";
    return node;
  }
}

const addLineHighlight = StateEffect.define();

const lineHighlightMark = Decoration.line({
  attributes: {style: 'background-color: #d2ffff'},
});

function App() {
  const instance = axios.create({
    baseURL: `http://${endpoint}`,
  });

  const [jobId, setJobId] = React.useState<null|number>(null);
  const [error, setError] = React.useState<null|any>(null);
  const [socket, setSocket] = React.useState<null|WebSocket>(null);
  const running = socket !== null;

  const [valueCode, setValueCode] = React.useState("");
  const onChangeCode = React.useCallback((val: any, viewUpdate: any) => {
    setValueCode(val);
  }, []);

  const editorRef = React.useRef<null|ReactCodeMirrorRef>(null);
  const emptyLineGutter = gutter({
    lineMarker(view, line) {
      console.log(error);
      if (error === null) return null;
      const lineNumber = view.state.doc.lineAt(line.from).number;
      if (lineNumber !== error.line) return null;
      return emptyMarker;
    },
    initialSpacer: () => emptyMarker
  });

  // const lineHighlightField = StateField.define({
  //   create() {
  //     return Decoration.none;
  //   },
  //   update(lines, tr) {
  //     lines = lines.map(tr.changes);
  //     for (let e of tr.effects) {
  //       if (e.is(addLineHighlight)) {
  //         lines = Decoration.none;
  //         lines = lines.update({add: [lineHighlightMark.range(e.value)]});
  //       }
  //     }
  //     return lines;
  //   },
  //   provide: (f) => EditorView.decorations.from(f),
  // });

  React.useEffect(() => {
    console.log(error);
    if (error === null) return;
    setValueOutput(error.message);
    // let effects = [];
    // effects.push(addUnderline.of({from: 1, to: 10}));
    // effects.push(StateEffect.appendConfig.of([emptyLineGutter]))
    // editorRef.current?.view?.dispatch({effects});
  }, [error, valueCode]);
  
  const inputRef = React.useRef<null|HTMLDivElement>(null);

  function ensureSelection() {
    const input = inputRef.current;
    if (input === null) return;

    const firstChild = input.firstChild;
    if (firstChild === null) {
      console.warn("invalid state");
      return;
    }
    const textLength = firstChild.nodeValue?.length;
    if (textLength === undefined) {
      console.warn("invalid state");
      return;
    }

    let range = new Range();
    range.setStart(firstChild, textLength);
    range.setEnd(firstChild, textLength);

    const selection = document.getSelection();
    selection?.removeAllRanges();
    selection?.addRange(range);
  }

  function ensureInputText() {
    const input = inputRef.current;
    if (input === null) return;

    let text = "";
    for (const node of Array.from(input.childNodes)) {
      if (node.nodeType === document.TEXT_NODE) {
        if (node.textContent !== null) {
          text = text.concat(node.textContent);
        }
      }
      node.remove();
    }

    text = text.replaceAll("\n", "");
    const node = document.createTextNode("");
    node.nodeValue = text;
    input.appendChild(node);
  }

  function ensureInputCursor() {
    const input = inputRef.current;
    if (input === null) return;

    const cursors = input.getElementsByClassName("cursor");
    if (cursors.length !== 1) {
      if (cursors.length > 1) {
        for (const cursor of Array.from(cursors)) {
          cursor.remove();
        }
      }

      const node = document.createElement("span");
      node.textContent = "█";
      node.classList.add("cursor");
      node.style.userSelect = "none";

      let before = input.firstChild?.nextSibling;
      if (before === undefined) before = null;
      
      input.insertBefore(node, before);
    }
  }

  React.useEffect(() => {
    ensureInputText();
    ensureInputCursor();
  }, []);
  
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
            setError(null);

            // const code = editorRef.current?.state?.doc.toString();
            // const code = editorRef.current?.editor?.nodeValue;
            console.log(editorRef);
            console.log(editorRef.current?.state);
            
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
                emptyLineGutter,
                // lineHighlightField,
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
              if (inputRef.current === null) return;
              inputRef.current.focus();
            }}
          >
            <div
              ref={outputRef}
              style={{
                whiteSpace: "break-spaces",
                overflowY: "auto",
              }}
            >{valueOutput}</div>
            <div
              ref={inputRef}
              spellCheck={false}
              contentEditable={true}
              style={{
                flexGrow: 1,
                flexBasis: "auto",
                
                caretColor: "transparent",
                padding: "0",
                border: "0",
                width: "100%",
                minHeight: "2ch",
                whiteSpace: "break-spaces",
              }}
              onSelect={(event) => {
                event.preventDefault();
              }}
              onBeforeInput={(_event) => {
                ensureInputText();
                ensureSelection();
              }}
              onInput={(event) => {
                ensureInputText();
                ensureSelection();
                ensureInputCursor();
              }}
              onKeyDown={(event) => {
                if (
                    event.key === "ArrowUp"
                  || event.key === "ArrowDown"
                  || event.key === "ArrowLeft"
                  || event.key === "ArrowRight"
                ) {
                  event.preventDefault();
                }
                
                if (event.key === "Enter") {
                  event.preventDefault();
                  if (socket === null) {
                    return;
                  }
                  const input = inputRef.current;
                  if (input === null) {
                    return;
                  }

                  const firstChild = input.firstChild;
                  if (firstChild === null) {
                    console.warn("invalid state");
                    return;
                  }
                  const text = firstChild.nodeValue;
                  if (text === undefined) {
                    console.warn("invalid state");
                    return;
                  }
                  socket.send(JSON.stringify({
                    message: {
                      type: "stdin",
                      stdin: `${text}\n`,
                    }
                  }));

                  firstChild.nodeValue = "";
                }
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
