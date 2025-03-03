FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && \
    apt-get install -y git gcc g++ make \
                       zlib1g-dev libncurses-dev liblzma-dev libbz2-dev \
                       libcurl4-gnutls-dev libcrypto++-dev libdeflate-dev && \
    git clone --recursive https://github.com/samtools/htslib.git && \
    git clone --recursive https://github.com/samtools/samtools.git && \
    cd samtools && make && make install && \
    rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=samtools
