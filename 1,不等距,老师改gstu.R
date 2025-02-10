library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

# 读取 Excel 数据
data <- read_excel("root GSTU.xlsx")

# 提取文件名从数据的第一列
filenames <- as.character(data[[1]])

# 定义一个函数，对每行数据计算平均值和标准误差
calculate_averages <- function(row) {
  
  # 获取第一组数据并计算每3个的平均值和标准误差
  values1 <- as.numeric(row[2:32])
  avg_values1 <- sapply(1:10, function(i) mean(values1[((i-1)*3+1):(i*3)]))
  se_values1 <- sapply(1:10, function(i) sd(values1[((i-1)*3+1):(i*3)])/sqrt(3))
  
  # 获取第二组数据并计算每3个的平均值和标准误差
  values2 <- c(as.numeric(row[2:4]), as.numeric(row[33:63]))
  avg_values2 <- sapply(1:10, function(i) mean(values2[((i-1)*3+1):(i*3)]))
  se_values2 <- sapply(1:10, function(i) sd(values2[((i-1)*3+1):(i*3)])/sqrt(3))
  
  # 创建一个数据框来保存平均值和标准误差
  df <- data.frame(
    Time = rep(c(0, 0.5, 1, 2, 4, 8, 12, 24, 48, 72), 2),
    Expression = c(avg_values1, avg_values2),
    SE = c(se_values1, se_values2),
    Group = factor(rep(1:2, each=10))
  )
  
  # 过滤掉横坐标为48和72的数据
  df <- df %>% filter(!(Time %in% c(48, 72)))
  
  return(df)
}

# 定义一个函数来生成图
generate_plot <- function(df, title_text) {
  p <- ggplot(df, aes(x = Time, y = Expression, color = Group, group = Group)) + 
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = Expression - SE, ymax = Expression + SE), width = 0.2) +
    labs(
      title = title_text,
      x = "Time (hours)", 
      y = "Transcript level (CPM)"
    ) +
    scale_color_manual(values = c("#2771a7","#d32421"), name = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, colour = "#FF8A5B"),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face="bold"),
      axis.title.y = element_text(size = 12, face="bold"),
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      aspect.ratio = 0.8
    ) +
    ylim(0, 1150) +
    scale_x_continuous(breaks=c(0,1,2,4,8,12,24))
  return(p)
}

# 使用数据的第一列作为标题来生成图
plot_list <- apply(data, 1, function(row) {
  avg_df <- calculate_averages(row)
  p <- generate_plot(avg_df, as.character(row[1]))
  return(p)
})

# 遍历 plot_list 并使用对应的文件名保存每一个图
for (i in 1:length(plot_list)) {
  ggsave(plot_list[[i]], file=paste0(filenames[i], ".png"), dpi=900, width=3, height=3)
}
