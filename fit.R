#install.packages("gginnards")
#install.packages("ggpmisc")
library(ggplot2)
library(ggpmisc)

# 设置工作路径
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 导入原始数据
dataExperiment6 <- read.csv("data/experiment6.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)

dataEx6_maize = subset(dataExperiment6, crop == "maize")
dataEx6_wheat = subset(dataExperiment6, crop == "wheat")

# scatterplot(root.length.m. ~ dose, data = dataEx6_maize,
#            spread=FALSE, smoother.args=list(lty=2), pch=19, main="root length", xlab = "dose", ylab = "root length")

ggplot(dataExperiment6, aes(dose, root.length.m., color = crop)) + 
  geom_point() + 
  geom_smooth(aes(fill = crop), method = "lm", formula = y ~ poly(x, 2, raw = TRUE)) + 
  theme_bw() + #设置主题
  stat_poly_eq(
    # paste里是latex形式公式
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = root.length.m. ~ poly(dose, 3, raw = TRUE),
    parse = TRUE, # 表示将这个语句翻译成可读形式
    size = 3 #公式字体大小
  ) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
