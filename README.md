# zugaı

A tool for parsing and interpreting Toaq text.

It can currently be invoked from [Discord](https://toaq.me/Discord) using `%tree Hıo ka` or `%logic Hıo ka` or (silly) `%english Hıo ka`.

## For programmers

### zugai-exe

This is the "core" of zugaı that does all the hard work.

To build, get [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and try `stack build`, `stack test`, `stack run zugai-exe`.

See `stack run zugai-exe -- --help` for command line instructions.

### Discord bot

A Python-based Discord bot that invokes the zugaı core.

Instructions:

1. Put `zugai-exe` on the PATH by running `stack install`
2. Put [`inkscape`](https://inkscape.org/) on the PATH
3. Run `python3 -m pip install discord`
4. Set an environment variable `export ZUGAI_DISCORD_TOKEN=<your bot token>`
5. Run `python3 discord_bot.py`

It responds to `%tree`/`%logic`/`%english` followed by a Toaq sentence.

### Web server

A Python-based HTTP server. `GET /zugai?text=jadi&to=english` responds with zugai's result as plain text.

Try `GET /zugai` to see the supported `&to=` formats.

Instructions:

1. Put `zugai-exe` on the PATH by running `stack install`
2. Run `python3 -m pip install flask`
3. Run `FLASK_APP=web_server python3 -m flask run`
