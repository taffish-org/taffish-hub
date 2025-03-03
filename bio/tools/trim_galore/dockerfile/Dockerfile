FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

RUN apt-get update && apt-get install -y curl fastqc cutadapt && \
    curl -fsSL https://github.com/FelixKrueger/TrimGalore/archive/0.6.10.tar.gz -o trim_galore.tar.gz && \
    tar -zxvf trim_galore.tar.gz && \
    cp TrimGalore-0.6.10/trim_galore /usr/local/bin/ && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /root

COPY . .

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=trim_galore
