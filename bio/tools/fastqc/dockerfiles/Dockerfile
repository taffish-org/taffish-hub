FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

RUN apt-get update && apt-get install -y fastqc

WORKDIR /root

COPY . .

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=fastqc
