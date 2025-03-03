FROM docker.io/library/python:3.12

LABEL maintainer="taffish@163.com"

WORKDIR /root

COPY . .

RUN pip install numpy==1.26.4 requests==2.32.3 pandas==2.2.2 && echo '#!/bin/sh' >> /usr/local/bin/uniprot-idmapping && echo 'python /root/uniprot-idmapping/main.py $@' > /usr/local/bin/uniprot-idmapping && chmod +x /usr/local/bin/uniprot-idmapping

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=uniprot-idmapping
