# 基础镜像
FROM docker.io/taffish/bior-base:latest

# 工作目录
WORKDIR /workspace

# 安装 BiocManager
RUN R -e "BiocManager::install('DESeq2'); if (!requireNamespace('DESeq2', quietly=TRUE)) quit(status=1)"
RUN R -e "BiocManager::install('edgeR'); if (!requireNamespace('edgeR', quietly=TRUE)) quit(status=1)"
RUN R -e "BiocManager::install('pheatmap'); if (!requireNamespace('pheatmap', quietly=TRUE)) quit(status=1)"
RUN R -e "BiocManager::install('limma'); if (!requireNamespace('limma', quietly=TRUE)) quit(status=1)"
# RUN R -e "BiocManager::install('clusterProfiler'); if (!requireNamespace('clusterProfiler', quietly=TRUE)) quit(status=1)"
RUN R -e "install.packages('Seurat')"
RUN R -e "install.packages('SeuratObject')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('patchwork')"
RUN R -e "install.packages('Matrix')"

COPY . .

ENV TAFFISH_ENV=TAFFISH
ENV TAFFISH_NAME=bior-rna
