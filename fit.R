#install.packages("gginnards")
#install.packages("ggpmisc")
library(ggplot2)
library(ggpmisc)
library(ggpubr)

# 设置工作路径
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 导入原始数据
dataExperiment6 <- read.csv("data/experiment6.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)

#dataEx6_maize = subset(dataExperiment6, crop == "maize")
#dataEx6_wheat = subset(dataExperiment6, crop == "wheat")

# 指数拟合
#fit_maize = nls(root.length.m. ~ A * exp(B * dose), data = dataEx6_maize, start = list(A = 1, B = 1))
#summary(lm_maize, plot = TRUE)
#a_maize = coefficients(lm_maize)[1]
#b_maize = coefficients(lm_maize)[2]
#conf_intervals_maize <- confint(lm_maize)

#fit_wheat = nls(root.length.m. ~ A * exp(B * dose), data = dataEx6_wheat, start = list(A = 1, B = 1))
#summary(lm_wheat)
#a_wheat = coefficients(lm_wheat)[1]
#b_wheat = coefficients(lm_wheat)[2]
#conf_intervals_wheat <- confint(lm_wheat)

#results_df <- data.frame(
#  Parameter = c("a", "b", "a", "b"),
#  Estimate = c(coef(fit_maize), coef(fit_wheat)),
#  Lower_CI = c(conf_intervals_maize[, 1], conf_intervals_wheat[, 1]),
#  Upper_CI = c(conf_intervals_maize[, 2], conf_intervals_wheat[, 2]),
#  crop = rep(c("maize", "wheat"), each = 2)
#)

# 创建拟合曲线数据
#curve_data <- data.frame(
#  x = seq(min(dataExperiment6$dose), max(dataExperiment6$dose), length.out = 100),
#  y_maize = exp(coefficients[1]) * exp(coefficients[2] * seq(min(data$x), max(data$x), length.out = 100))
#)

#x = (1:1000)/100
#fitData = data.frame(
#  x = x
#)
#ggplot(dataExperiment6, aes(dose, root.length.m., color = crop)) + 
#  geom_point() + 
#  theme_bw() + #设置主题
#  stat_function(fun = function(x) a_maize * exp(b_maize * x), color = "#909090") +
#  stat_function(fun = function(x) a_wheat * exp(b_wheat * x), color = "#404040") + 
#  scale_color_manual(values = c("#909090", "#404040")) + 
#  scale_color_manual(values = c("#909090", "#404040"))
  



# scatterplot(root.length.m. ~ dose, data = dataEx6_maize,
#            spread=FALSE, smoother.args=list(lty=2), pch=19, main="root length", xlab = "dose", ylab = "root length")

formula = y ~ x
formula2 = y ~ poly(x, 2, raw = TRUE)
ggplot(dataExperiment6, aes(dose, root.length.m., color = crop)) + 
  geom_point() + 
  geom_smooth(aes(fill = crop), method = "lm", formula = formula2, fullrange = FALSE, level = 0.95) + 
  theme_bw() + #设置主题
  stat_poly_eq(
    # paste里是latex形式公式
    use_label(c("eq", "R2", "adj.R2", "p.value.label")),
    formula = formula2,
    parse = TRUE, # 表示将这个语句翻译成可读形式
    size = 4 #公式字体大小
  ) + 
  scale_fill_manual(values = c("#909090", "#404040")) + 
  scale_color_manual(values = c("#909090", "#404040"))

ggplot(dataExperiment6, aes(dose, root.length.m., color = crop, linetype = crop)) + 
  geom_point() + 
  geom_smooth(aes(fill = crop), method = "lm", formula = formula) + 
  theme_bw() + #设置主题
  scale_fill_manual(values = c("#404040", "#909090")) + 
  scale_color_manual(values = c("#404040", "#909090"))

# 指数拟合
ggplot(dataExperiment6, aes(dose, root.length.m., color = crop)) + 
  geom_point() + 
  geom_smooth(aes(fill = crop), method = "nls", formula = y ~ A * exp(B * x), method.args = list(start = c(A = 1, B = 1)), se = FALSE, level = 0.95) +
  theme_bw() + #设置主题
  scale_fill_manual(values = c("#909090", "#404040")) + 
  scale_color_manual(values = c("#909090", "#404040")) + 
  ggpubr::stat_cor(aes(color = crop), label.x = 3)

