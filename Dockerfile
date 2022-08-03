FROM debian AS compile
RUN apt update
RUN apt install -y curl zlib1g-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

# prefetch ghc
RUN stack ghci </dev/null

COPY stack.yaml stack.yaml.lock package.yaml Setup.hs /pkg/
RUN cd /pkg/ \
   && stack build --only-dependencies

COPY src/ /pkg/src/
COPY app/ /pkg/app/
COPY test/ /pkg/test/
RUN cd /pkg/ \
  && stack install \
  && cp /root/.local/bin/zugai-exe /pkg/zugai-exe


FROM debian AS base
RUN apt update; apt install -y python3 python3-pip
COPY data/ /pkg/data/


FROM base AS bot
RUN apt install -y inkscape fonts-linuxlibertine texlive-xetex
# fix imagemagick's restrictive PostScript policy - this is fine since we're running in a container with semi-controlled inputs
RUN pip3 install discord
RUN sed -i '/PS\|PDF/s/none/read|write/' /etc/ImageMagick-*/policy.xml
COPY discord_bot.py /pkg/
COPY --from=compile /pkg/zugai-exe /pkg/
ENTRYPOINT cd /pkg/ \
  && PATH=/pkg/:"$PATH" python3 ./discord_bot.py


FROM base AS web
EXPOSE 80
RUN pip3 install flask
COPY web_server.py /pkg/
COPY --from=compile /pkg/zugai-exe /pkg/
ENTRYPOINT cd /pkg/ \
  && PATH=/pkg/:"$PATH" FLASK_APP=web_server python3 -m flask run -h 0.0.0.0 -p 80
