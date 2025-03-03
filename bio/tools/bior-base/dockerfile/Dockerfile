# 基础镜像
FROM rocker/r-ver:4.4.2

# 工作目录
WORKDIR /workspace

COPY . .

# 安装系统依赖
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    zlib1g-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libgfortran5 \
    libhdf5-dev \
    libglpk-dev \
    && apt-get clean

# 安装 BiocManager
RUN R -e "install.packages('BiocManager')"
RUN R -e "install.packages('ggplot2'); if (!requireNamespace('ggplot2', quietly=TRUE)) quit(status=1)"

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=bior-base-4.4.2
