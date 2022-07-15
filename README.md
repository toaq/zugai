# zugaı
A tool for parsing and interpreting Toaq text.

It can currently be invoked from [Discord](https://toaq.me/Discord) using `%tree Hıo ka` or `%logic Hıo ka` or (silly) `%english Hıo ka`.

## For programmers

To build, get [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and try `stack build`, `stack test`, `stack run`.

See `stack run -- --help` for command line instructions.

To run the Discord bot:
1. Put `zugai-exe` on the PATH by running `stack install`
2. Put [`inkscape`](https://inkscape.org/) on the PATH
3. Run `python3 -m pip install discord`
4. Set an environment variable `export ZUGAI_DISCORD_TOKEN=<your bot token>`
5. Run `python3 discord_bot.py`

To run the web server:
1. Put `zugai-exe` on the PATH by running `stack install`
2. Run `python3 -m pip install flask`
3. Run `FLASK_APP=web_server python3 -m flask run`

