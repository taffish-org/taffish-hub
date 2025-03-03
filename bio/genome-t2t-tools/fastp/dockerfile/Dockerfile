FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN apt-get update && \
    apt-get install -y git gcc g++ make libisal-dev libdeflate-dev && \
    git clone https://github.com/OpenGene/fastp.git && \
    cd fastp && make -j && make install && cp fastp /usr/local/bin/

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=fastp
