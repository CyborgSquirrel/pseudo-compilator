import React, { useEffect } from "react";

interface CommandLineProps {
  value: string,
  setValue: React.Dispatch<React.SetStateAction<string>>,
  onSubmit?: (value: string) => void,

  style?: React.CSSProperties,
}

const CommandLine: React.ForwardRefRenderFunction<HTMLDivElement|null, CommandLineProps> = (props, forwardedRef) => {
  const {value, setValue} = props;

  // NOTE: Have to make Typescript forget the type of the ref, then tell it the
  // type myself, because otherwise it goes completely off the rails.
  const fallbackRef = React.useRef<HTMLDivElement|null>(null);
  const refUntyped: any = forwardedRef || fallbackRef;
  const ref: React.MutableRefObject<HTMLDivElement|null> = refUntyped;

  const [isActive, setIsActive] = React.useState(ref.current === document.activeElement);

  useEffect(() => {
    const input = ref.current;
    if (input === null) return;

    fixInput(value);
  }, [value]);

  function getInputValue() {
    const input = ref.current;
    if (input === null) return "";

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
    return text;
  }

  function fixInput(value: string) {
    const input: HTMLDivElement|null = ref.current;
    if (input === null) return;

    for (const node of Array.from(input.childNodes)) {
      node.remove();
    }

    // Fix the text
    const textNode = document.createTextNode(value);
    input.appendChild(textNode);

    // Fix the selection
    const range = new Range();
    range.setStart(textNode, textNode.length);
    range.setEnd(textNode, textNode.length);

    const selection = document.getSelection();
    selection?.removeAllRanges();
    selection?.addRange(range);
  }

  return (
    <div
      // NOTE: Disable spell check because it keeps flickering, due to the fact
      // that the text inside this div is constantly getting reset.
      spellCheck={false}

      style={props.style}

      ref={ref}
      contentEditable={true}
      className={`aj-command-line ${isActive ? "aj-active" : ""}`}
      onSelect={(event) => {
        event.preventDefault();
      }}
      onBeforeInput={(_event) => {
        fixInput(value);
      }}
      onInput={(_event) => {
        setValue(getInputValue());
      }}
      onFocus={() => {
        setIsActive(true);
      }}
      onBlur={() => {
        setIsActive(false);
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
          if (props.onSubmit !== undefined) {
            props.onSubmit(value);
          }
        }
      }}
    />
  );
};

export default React.forwardRef<HTMLDivElement | null, CommandLineProps>(CommandLine);
