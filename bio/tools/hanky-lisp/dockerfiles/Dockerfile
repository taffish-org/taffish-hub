FROM docker.io/fukamachi/sbcl:2.4.10

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN mv /root/hanky-lisp /usr/local/bin/ && chmod +x /usr/local/bin/hanky-lisp

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=hanky-lisp
