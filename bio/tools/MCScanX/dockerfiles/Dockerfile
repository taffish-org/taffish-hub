FROM docker.io/library/debian:12

LABEL maintainer="taffish@163.com"

RUN apt-get update && apt-get install -y gcc g++ make default-jdk && rm -rf /var/lib/apt/lists/*

WORKDIR /root

COPY . .

RUN cd /root/MCScanX-master && make && cd /usr/local/bin && ln -s /root/MCScanX-master/MCScanX MCScanX

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=MCScanX
