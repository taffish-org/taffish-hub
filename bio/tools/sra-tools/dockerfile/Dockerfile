FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && \
    apt-get install -y git gcc g++ make cmake python3 \
    libfuse-dev libhdf5-dev libmagic-dev libxml2-dev && \
    git clone --recursive https://github.com/ncbi/sra-tools.git && \
    git clone --recursive https://github.com/ncbi/ncbi-vdb.git && \
    cd ncbi-vdb && ./configure && make && make install && \
    cd ../sra-tools && ./configure && make && make install && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/usr/local/ncbi/sra-tools/bin:$PATH

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=sra-tools
