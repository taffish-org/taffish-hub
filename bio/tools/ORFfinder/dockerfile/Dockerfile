FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && apt-get install -y libuv1-dev libnghttp2-dev libdw-dev && cp ORFfinder /usr/local/bin/ && rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=orffinder
