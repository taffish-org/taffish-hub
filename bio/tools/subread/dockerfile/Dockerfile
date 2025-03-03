FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && \
    apt-get install -y git gcc g++ make zlib1g-dev && \
    git clone --recursive https://github.com/ShiLab-Bioinformatics/subread && \
    cd subread/src && make -f Makefile.Linux && \
    mv ../bin/utilities/* ../bin/ && \
    rmdir ../bin/utilities && \
    cp ../bin/* /usr/local/bin/ && \
    rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=subread
