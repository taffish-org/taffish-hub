FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && apt-get install -y bowtie2 && rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=bowtie2
