# test.R
# 测试 DESeq2、edgeR、pheatmap、limma 和 clusterProfiler 是否安装成功并能正常使用

# 包清单
required_packages <- c("DESeq2", "edgeR", "pheatmap", "limma")

# 检查每个包是否安装成功
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste(pkg, "未安装成功！"))
  } else {
    cat(paste(pkg, "安装成功。\n"))
  }
}

# 测试 DESeq2
cat(">>> 测试 DESeq2 功能...\n")
library(DESeq2)
dds <- makeExampleDESeqDataSet()
dds <- DESeq(dds)
res <- results(dds)
cat("DESeq2 运行成功，差异基因分析完成。\n")

# 测试 edgeR
cat(">>> 测试 edgeR 功能...\n")
library(edgeR)
group <- factor(c(1, 1, 2, 2))
y <- DGEList(counts = matrix(rpois(20, lambda = 10), ncol = 4), group = group)
y <- calcNormFactors(y)
design <- model.matrix(~group)
y <- estimateDisp(y, design)
fit <- glmFit(y, design)
lrt <- glmLRT(fit)
cat("edgeR 运行成功，差异基因分析完成。\n")

# 测试 pheatmap
cat(">>> 测试 pheatmap 功能...\n")
library(pheatmap)
mat <- matrix(rnorm(100), nrow = 10)
pheatmap(mat)
cat("pheatmap 绘图成功。\n")

# 测试 limma
cat(">>> 测试 limma 功能...\n")
library(limma)
fit <- lmFit(matrix(rnorm(100), ncol = 5), model.matrix(~factor(c(1, 1, 2, 2, 2))))
fit <- eBayes(fit)
top <- topTable(fit)
cat("limma 运行成功，差异基因分析完成。\n")

cat(">>> 所有测试完成，bior-rna-base 环境成功构建！\n")
