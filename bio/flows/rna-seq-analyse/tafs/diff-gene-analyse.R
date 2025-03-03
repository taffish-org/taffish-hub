suppressMessages(library(DESeq2))
suppressMessages(library(ggplot2))
suppressMessages(library(edgeR))
suppressMessages(library(pheatmap))
suppressMessages(library(limma))

## 输入定量结果文件
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("Please offer 4 args:\nUsage: Rscript my_script.R <feature_counts_file> <the_work_dir> <p_value_threshold> <log_fc_threshold> <if_adj>")
}
feature_counts_file <- args[1]
the_work_dir        <- args[2]
p_value_threshold   <- abs(as.numeric(args[3]))
log_fc_threshold    <- abs(as.numeric(args[4]))
if_adj              <- (args[5] == "1")

# 设置工作目录
setwd(the_work_dir)

if (if_adj) {
    cat("\n本次运行参数：<是否使用矫正 p 值: 是>  <p-value: ", p_value_threshold, ">  <log_fc: ", log_fc_threshold, ">\n\n")
} else {
    cat("\n本次运行参数：<是否使用矫正 p 值: 否>  <p-value: ", p_value_threshold, ">  <log_fc: ", log_fc_threshold, ">\n\n")
}

# ---------------------------
# Step 1: 读取和预处理数据
# ---------------------------

# 读取 featureCounts 的输出文件
count_data <- read.delim(feature_counts_file, comment.char = "#", row.names = 1)

# 提取计数矩阵（跳过前 6 列非样本数据）
count_data <- count_data[, 6:ncol(count_data)]

# 修正列名（去掉路径和后缀）
colnames(count_data) <- gsub(".*/", "", colnames(count_data))  # 去掉路径前缀
colnames(count_data) <- gsub(".Aligned.sortedByCoord.out.bam", "", colnames(count_data))  # 去掉后缀

# 样本信息表（修改为你的实验设计）
sample_info <- data.frame(
  row.names = colnames(count_data),
  condition = factor(c("control", "control", "treatment", "treatment"))  # 修改为实际分组
)

# ---------------------------
# Step 2: 差异表达分析 (DESeq2)
# ---------------------------

# 构建 DESeq2 对象
dds <- DESeqDataSetFromMatrix(countData = count_data, colData = sample_info, design = ~ condition)

# 运行差异表达分析
dds <- DESeq(dds)
res <- results(dds, alpha = p_value_threshold)  # 校正 p 值阈值为 0.05
res <- res[order(res$padj), ]  # 按校正 p 值排序

# 筛选显著差异基因
#res_sig <- subset(res, padj < 0.05 & abs(log2FoldChange) > 2)
if (if_adj) {
    res_sig <- subset(res, padj   < p_value_threshold & abs(log2FoldChange) > log_fc_threshold)
} else {
    res_sig <- subset(res, pvalue < p_value_threshold & abs(log2FoldChange) > log_fc_threshold)
}

# 保存结果
write.csv(as.data.frame(res), file = "./deseq2_results.csv")
write.csv(as.data.frame(res_sig), file = "./deseq2_significant_results.csv")

cat("  DESeq2 显著差异基因数目：", nrow(res_sig), "\n")

# ---------------------------
# Step 3: 可视化 1 - 火山图
# ---------------------------

res_df <- as.data.frame(res)
res_df$gene <- rownames(res)
res_df$significance <- "Not Significant"
#res_df$significance[res_df$padj < 0.05 & abs(res_df$log2FoldChange) > 1] <- "Significant"
if (if_adj) {
    res_df$significance[res_df$padj   < p_value_threshold & res_df$log2FoldChange > log_fc_threshold]        <- "UpRegulated"
    res_df$significance[res_df$padj   < p_value_threshold & res_df$log2FoldChange < (-1 * log_fc_threshold)] <- "DownRegulated"
} else {
    res_df$significance[res_df$pvalue < p_value_threshold & res_df$log2FoldChange > log_fc_threshold]        <- "UpRegulated"
    res_df$significance[res_df$pvalue < p_value_threshold & res_df$log2FoldChange < (-1 * log_fc_threshold)] <- "DownRegulated"
}

res_df <- na.omit(res_df)

volcano_plot <- ggplot(res_df, aes(x = log2FoldChange, y = -log10(pvalue), color = significance)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(
    values = c(
      "UpRegulated" = "red",
      "DownRegulated" = "blue",
      "Not Significant" = "grey"
    )
  ) +
  labs(
    title = "Volcano Plot",
    x = "Log2 Fold Change",
    y = "-Log10 P-value",
    color = "Significance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggsave("./volcano_plot.png", plot = volcano_plot, width = 8, height = 6)

# ---------------------------
# Step 4: 数据标准化 (logCPM)
# ---------------------------

# 使用 edgeR 的 CPM 方法对数据标准化
dge <- DGEList(counts = count_data)
dge <- calcNormFactors(dge)  # 计算归一化因子
log_cpm <- cpm(dge, log = TRUE)  # 计算 logCPM

# PCA 分析
# 使用 rlog 替代 vst
rld <- rlog(dds, blind = FALSE)

# PCA 分析
pca_data <- plotPCA(rld, intgroup = "condition", returnData = TRUE)
percentVar <- round(100 * attr(pca_data, "percentVar"))

pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = condition)) +
  geom_point(size = 3) +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  ggtitle("PCA of Samples") +
  theme_minimal(base_size = 14)
ggsave("./pca_plot.png", plot = pca_plot, width = 8, height = 6)

# ---------------------------
# Step 5: 可视化 2 - DESeq2 热图
# ---------------------------

# 确保有显著差异基因
if (nrow(res_sig) == 0) {
  stop("没有显著差异基因，无法生成热图！")
}

colnames(log_cpm) <- gsub("^.+\\.STAR\\.", "", colnames(log_cpm))  # 去掉路径和前缀
colnames(log_cpm) <- gsub("\\.sub$", "", colnames(log_cpm))       # 去掉末尾的 .sub

heatmap_data <- log_cpm[rownames(res_sig), ]

# 如果数据范围不足，可以人为设置范围
if (nrow(heatmap_data) > 0) {
  pheatmap(
    heatmap_data,
    scale = "row",
    cluster_cols = TRUE,
    cluster_rows = TRUE,
    show_rownames = FALSE,
    main = "Significant DE Genes (logCPM)",
    color = colorRampPalette(c("blue", "white", "red"))(50)
  )
} else {
  stop("热图数据为空，无法绘制热图！")
}

# ---------------------------
# Step 6: 差异表达分析 (limma)
# ---------------------------

# 构建模型矩阵
design <- model.matrix(~condition, data = sample_info)

# voom 转换
v <- voom(dge, design, plot = TRUE)

# 拟合线性模型
fit <- lmFit(v, design)
fit <- eBayes(fit)

# 提取差异基因
limma_res <- topTable(fit, coef = 2, adjust = "BH", number = Inf)
#limma_res_sig <- subset(limma_res, adj.P.Val < 0.05 & abs(logFC) > 1)
if (if_adj) {
    limma_res_sig <- subset(limma_res, adj.P.Val < p_value_threshold & abs(logFC) > log_fc_threshold)
} else {
    limma_res_sig <- subset(limma_res, P.Value   < p_value_threshold & abs(logFC) > log_fc_threshold)
}

# 保存 limma 差异分析结果
write.csv(limma_res, file = "./limma_results.csv")
write.csv(limma_res_sig, file = "./limma_significant_results.csv")

cat("  Limma 显著差异基因数目：", nrow(limma_res_sig), "\n")

# ---------------------------
# step 7: 可视化 3 - limma 火山图
# ---------------------------

limma_res$significance <- "Not Significant"
#limma_res$significance[limma_res$adj.P.Val < 0.05 & abs(limma_res$logFC) > 1] <- "Significant"
if (if_adj) {
    limma_res$significance[limma_res$adj.P.Val < p_value_threshold & limma_res$logFC > log_fc_threshold]        <- "UpRegulated"
    limma_res$significance[limma_res$adj.P.Val < p_value_threshold & limma_res$logFC < (-1 * log_fc_threshold)] <- "DownRegulated"
} else {
    limma_res$significance[limma_res$P.Value   < p_value_threshold & limma_res$logFC > log_fc_threshold]        <- "UpRegulated"
    limma_res$significance[limma_res$P.Value   < p_value_threshold & limma_res$logFC < (-1 * log_fc_threshold)] <- "DownRegulated"
}

limma_volcano <- ggplot(limma_res, aes(x = logFC, y = -log10(P.Value), color = significance)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(
    values = c(
      "UpRegulated" = "red",
      "DownRegulated" = "blue",
      "Not Significant" = "grey"
    )
  ) +
  labs(
    title = "Volcano Plot (limma)",
    x = "Log Fold Change",
    y = "-Log10 P-value",
    color = "Significance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggsave("./limma_volcano_plot.png", plot = limma_volcano, width = 8, height = 6)

# ---------------------------
# Step 8: 可视化 4 - limma 热图
# ---------------------------

# 确保有显著差异基因
if (nrow(limma_res_sig) == 0) {
  stop("没有显著差异基因，无法生成热图！")
}

#colnames(log_cpm) <- gsub("^.+\\.STAR\\.", "", colnames(log_cpm))  # 去掉路径和前缀
#colnames(log_cpm) <- gsub("\\.sub$", "", colnames(log_cpm))       # 去掉末尾的 .sub

heatmap_data <- log_cpm[rownames(limma_res_sig), ]

# 绘制热图
pheatmap(
  heatmap_data,
  scale = "row",
  cluster_cols = TRUE,
  cluster_rows = TRUE,
  show_rownames = FALSE,
  main = "Significant DE Genes (limma, logCPM)",
  color = colorRampPalette(c("blue", "white", "red"))(50)
)

# ---------------------------
# Step 9: 比较 DESeq2 与 limma
# ---------------------------

common_genes <- intersect(rownames(res_sig), rownames(limma_res_sig))
cat("  DESeq2 与 limma 共同显著差异基因数目：", length(common_genes), "\n")

cat("\n  DESeq2 上调基因：", sum(res_sig$log2FoldChange > 0), "\n")
cat("  DESeq2 下调基因：", sum(res_sig$log2FoldChange < 0), "\n\n")

# ---------------------------
# Step 10: 可视化 5 - 维恩图
# ---------------------------

# 从 DESeq2 和 limma 结果提取显著基因名
deseq_genes <- rownames(res_sig)
limma_genes <- rownames(limma_res_sig)

# 计算交集和差异集
common_genes <- intersect(deseq_genes, limma_genes)
deseq_only <- setdiff(deseq_genes, limma_genes)
limma_only <- setdiff(limma_genes, deseq_genes)

# 基因集大小
length_deseq <- length(deseq_genes)
length_limma <- length(limma_genes)
length_common <- length(common_genes)

library(tibble)

# 构造数据框
venn_data <- tibble(
  Gene = c(deseq_only, limma_only, common_genes),
  Group = c(
    rep("DESeq2 Only", length(deseq_only)),
    rep("limma Only", length(limma_only)),
    rep("Common", length(common_genes))
  )
)

library(ggplot2)

# 绘制维恩图
venn_plot <- ggplot(venn_data, aes(x = Group, fill = Group)) +
  geom_bar(width = 0.7, alpha = 0.8) +
  labs(
    title = "Venn Diagram of Differentially Expressed Genes",
    x = NULL,
    y = "Number of Genes",
    fill = "Gene Group"
  ) +
  scale_fill_manual(values = c("DESeq2 Only" = "red", "limma Only" = "blue", "Common" = "purple")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

# 保存图像
ggsave("venn_plot.png", plot = venn_plot, width = 8, height = 6)


cat("分析和可视化完成！结果已保存到文件夹。\n")
