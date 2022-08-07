FROM debian AS exe-build
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
  && cp /root/.local/bin/zugai-exe /usr/bin/zugai-exe


FROM debian AS base
RUN apt update; apt install -y python3 python3-pip inkscape fonts-linuxlibertine texlive-xetex
# relax imagemagick's restrictive PostScript policy
RUN sed -i '/PS\|PDF/s/none/read|write/' /etc/ImageMagick-*/policy.xml
COPY data/ /pkg/data/


FROM base AS bot
RUN pip3 install discord
COPY discord_bot.py zugai.py /pkg/
COPY --from=exe-build /usr/bin/zugai-exe /usr/bin/zugai-exe
ENTRYPOINT cd /pkg/ \
  && python3 ./discord_bot.py


FROM debian as web-build
RUN apt update; apt install -y curl
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - \
  && apt install -y nodejs
COPY web-client/package-lock.json /pkg/web-client/package-lock.json
COPY web-client/package.json /pkg/web-client/package.json
RUN cd /pkg/web-client/ \
  && npm install
COPY web-client /pkg/web-client/
RUN cd /pkg/web-client/ \
  && npm run build


FROM base AS web
EXPOSE 80
RUN pip3 install flask gunicorn
COPY web_server.py zugai.py /pkg/
COPY --from=exe-build /usr/bin/zugai-exe /usr/bin/zugai-exe
COPY --from=web-build /pkg/web-client/build/ /pkg/web-client/build/
ENTRYPOINT cd /pkg/ \
  && gunicorn -w `nproc` -b '0.0.0.0:80' 'web_server:app'
