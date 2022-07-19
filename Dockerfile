FROM debian AS compile
RUN apt update
RUN apt install -y curl zlib1g-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

# prefetch ghc
RUN stack ghci </dev/null

COPY stack.yaml stack.yaml.lock package.yaml Setup.hs /src/
RUN cd /src/; stack build --only-dependencies

COPY . /src
RUN cd /src \
  && stack install \
  && cp /root/.local/bin/zugai-exe /src/


FROM debian AS base
RUN apt update; apt install -y python3 python3-pip


FROM base AS bot
RUN apt install -y inkscape
RUN pip3 install discord
COPY --from=compile /src /src
COPY --from=compile /root/.stack/snapshots /root/.stack/snapshots
ENTRYPOINT cd /src \
  && PATH=/src:"$PATH" python3 ./discord_bot.py


FROM base AS web
EXPOSE 80
RUN pip3 install flask
COPY --from=compile /src /src
COPY --from=compile /root/.stack/snapshots /root/.stack/snapshots
ENTRYPOINT cd /src \
  && FLASK_APP=web_server PATH=/src:"$PATH" python3 -m flask run -h 0.0.0.0 -p 80
