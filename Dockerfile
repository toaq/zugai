FROM debian AS compile
ARG STACK_CMD=stack --verbosity=0 --jobs=`nproc`
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update -q \
 && apt install -qy zlib1g-dev

COPY stack.yaml stack.yaml.lock package.yaml Setup.hs /pkg/
ADD https://get.haskellstack.org/ /stack.sh
RUN cd /pkg/ \
 && sh /stack.sh -q \
 && $STACK_CMD build --only-dependencies

COPY app/ /pkg/app/
COPY etc/ /pkg/etc/
COPY src/ /pkg/src/
COPY test/ /pkg/test/
RUN cd /pkg/ \
 && $STACK_CMD install \
 && cp /root/.local/bin/zugai-exe /usr/bin/zugai-exe


FROM alpine
RUN apk add --no-cache gcompat python3 py3-flask py3-gunicorn imagemagick ttf-linux-libertine texlive-xetex texmf-dist-latexextra texmf-dist-pictures
COPY data/ /pkg/data/
COPY web_server.py zugai.py /pkg/
COPY --from=compile /usr/bin/zugai-exe /usr/bin/zugai-exe
EXPOSE 80
CMD cd /pkg/ \
 && gunicorn -w `nproc` -b 0.0.0.0:80 web_server:app
