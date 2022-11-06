import React, { ReactElement, FormEvent, useEffect, useState } from "react";
import "./App.css";

function App() {
  const [outputMode, setOutputMode] = useState<string>("boxes-flat");
  const [inputText, setInputText] = useState<string>("Kảı súq sa shou.");
  const [latestOutput, setLatestOutput] = useState<ReactElement>(<>
    Output will appear here.
  </>);
  function get(e: FormEvent<HTMLFormElement>) {
    e.preventDefault();
    setLatestOutput(<>...</>);
    fetch(
      "https://zugai.toaq.me/zugai?" +
        new URLSearchParams({ to: outputMode, text: inputText })
    ).then(async (result) => {
      const contentType = result.headers.get("Content-Type")!;
      if(contentType.startsWith("image/png")) {
        const body = await result.blob();
        setLatestOutput(<img src={URL.createObjectURL(body)} />);
      } else if(contentType.startsWith("text/")) {
        const body = await result.text();
        if(contentType.startsWith("text/html")) {
          setLatestOutput(<iframe
            className="boxes-output"
            srcDoc={body}
            title="html output"
          />);
        } else {
          setLatestOutput(<>{body}</>);
        }
      }
    });
  }
  return (
    <div className="zugai">
      <h1>mí Zugaı</h1>
      <div className="card settings">
        <form onSubmit={get}>
          <label>
            Output mode:&nbsp;
            <select
              value={outputMode}
              onChange={(e) => setOutputMode(e.target.value)}
            >
              <option value="boxes-flat">Boxes (flat)</option>
              <option value="boxes-nested">Boxes (nested)</option>
              <option value="english">English</option>
              <option value="logic">Logic</option>
              <option value="structure">Structure</option>
              <option value="xbar-png">X-bar tree</option>
            </select>
          </label>
          <textarea
            rows={3}
            value={inputText}
            onChange={(e) => setInputText(e.target.value)}
          />
          <button type="submit">Submit</button>
        </form>
      </div>
      <div className="card output">
        {latestOutput}
      </div>
    </div>
  );
}

export default App;
