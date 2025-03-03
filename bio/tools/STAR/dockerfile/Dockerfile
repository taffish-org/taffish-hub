FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && \
    apt-get install -y git gcc g++ make xxd \
                       zlib1g-dev && \
    git clone --recursive https://github.com/alexdobin/STAR.git && \
    cd STAR/source && \
    make STAR CXXFLAGS_SIMD="" && \
    cp STAR /usr/local/bin/ && \
    rm -rf /var/lib/apt/lists/*

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=STAR
