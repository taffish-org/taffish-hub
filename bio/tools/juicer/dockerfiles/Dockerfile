FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

RUN apt-get update && apt-get install -y default-jdk && rm -rf /var/lib/apt/lists/*

WORKDIR /root

COPY . .

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=juicer
