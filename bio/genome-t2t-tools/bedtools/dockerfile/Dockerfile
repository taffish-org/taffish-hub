FROM docker.io/library/python:3.12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && apt-get install -y gcc g++ make zlib1g-dev libbz2-dev liblzma-dev && tar -zxvf bedtools-2.31.1.tar.gz && cd bedtools2 && make && cp bin/* /usr/local/bin/ && rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=bedtools
