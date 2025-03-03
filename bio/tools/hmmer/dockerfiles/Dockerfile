FROM docker.io/library/python:3.12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN cd hmmer-3.4 && ./configure --prefix /usr/local && make && make check && make install && cd easel && make install

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=hmmer
