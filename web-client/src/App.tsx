import React, { FormEvent, useEffect, useState } from "react";
import "./App.css";

function App() {
  const [outputMode, setOutputMode] = useState<string>("english");
  const [inputText, setInputText] = useState<string>("Kảı súq sa shou.");
  const [latestOutput, setLatestOutput] = useState<string>(
    "Output will appear here."
  );
  function get(e: FormEvent<HTMLFormElement>) {
    e.preventDefault();
    setLatestOutput("...");
    fetch("zugai?" +
      new URLSearchParams({ to: outputMode, text: inputText })
    ).then(async (result) => {
      console.log(result);
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
              <option value="boxes">Boxes</option>
              <option value="english">English</option>
              <option value="logic">Logic</option>
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
      <div className="card output">{latestOutput}</div>
    </div>
  );
}

export default App;
