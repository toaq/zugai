import React, { FormEvent, useEffect, useState } from "react";
import "./App.css";

function App() {
  const [outputMode, setOutputMode] = useState<string>("english");
  const [inputText, setInputText] = useState<string>("Kảı súq sa ba.");
  const [latestOutput, setLatestOutput] = useState<string>(
    "Output will appear here."
  );
  function get(e: FormEvent<HTMLFormElement>) {
    e.preventDefault();
    setLatestOutput("...");
    fetch(
      "https://zugai.toaq.me/zugai?" +
        new URLSearchParams({ to: outputMode, text: inputText })
    ).then(async (result) => {
      console.log(result);
    });
  }
  return (
    <div className="App">
      <div className="settings">
        <form onSubmit={get}>
          <label>
            Output mode:
            <select
              value={outputMode}
              onChange={(e) => setOutputMode(e.target.value)}
            >
              <option value="boxes">Boxes</option>
              <option value="english">English</option>
              <option value="logic">Logic</option>
            </select>
          </label>
          <br />
          <textarea
            value={inputText}
            onChange={(e) => setInputText(e.target.value)}
          />
          <br />
          <button type="submit">Submit</button>
        </form>
      </div>
      <div className="output">{latestOutput}</div>
    </div>
  );
}

export default App;
