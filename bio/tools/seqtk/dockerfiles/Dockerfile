FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

RUN apt-get update && apt-get install -y git gcc g++ make zlib1g-dev && \
    git clone --recursive https://github.com/lh3/seqtk.git && \
    cd seqtk && make && cp seqtk /usr/local/bin/

WORKDIR /root

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=seqtk
