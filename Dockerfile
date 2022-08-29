FROM debian AS compile
RUN apt update \
 && apt install -y curl zlib1g-dev \
 && curl -sSL https://get.haskellstack.org/ | sh \
 && stack ghci </dev/null

COPY stack.yaml stack.yaml.lock package.yaml Setup.hs /pkg/
RUN cd /pkg/ \
 && stack build --only-dependencies

COPY app/ /pkg/app/
COPY etc/ /pkg/etc/
COPY src/ /pkg/src/
COPY test/ /pkg/test/
RUN cd /pkg/ \
 && stack install \
 && cp /root/.local/bin/zugai-exe /usr/bin/zugai-exe


FROM alpine
RUN apk add --no-cache gcompat python3 py3-flask py3-gunicorn imagemagick ttf-linux-libertine texlive-xetex texmf-dist-latexextra texmf-dist-pictures
COPY data/ /pkg/data/
COPY web_server.py zugai.py /pkg/
COPY --from=compile /usr/bin/zugai-exe /usr/bin/zugai-exe
EXPOSE 80
ENTRYPOINT cd /pkg/ \
        && gunicorn -w `nproc` -b 0.0.0.0:80 web_server:app
