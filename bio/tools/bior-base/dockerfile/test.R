# test_installation.R
# 测试 BiocManager 和 ggplot2 是否可用

# 加载 BiocManager
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  stop("BiocManager 未安装成功！")
} else {
  cat("BiocManager 安装成功。\n")
}

# 加载 ggplot2
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 未安装成功！")
} else {
  cat("ggplot2 安装成功。\n")
}

# 使用 ggplot2 绘制一个简单的图
library(ggplot2)

# 创建示例数据
data <- data.frame(
  x = 1:10,
  y = (1:10)^2
)

# 绘图
p <- ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  ggtitle("测试 ggplot2 成功！")
print(p)

cat("测试完成：BiocManager 和 ggplot2 功能正常。\n")

