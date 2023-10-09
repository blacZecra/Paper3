# 设置工作路径
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 以下为程序运行所需要的包，放入变量中通过LoadPkgs.R加载
tidyverse_pkgs <- c("tidyverse")

datawrangling_pkgs <- c("grid","reshape2","rlist") 

graphic_pkgs <- c("gridExtra","scales","RColorBrewer", "corrplot","tiff")

stats_pkgs <- c("vegan","betapart","car","emmeans","Hmisc","multcomp","multcompView")

archive_pkgs <- c("vegetarian")
archive_pkgs_version <- c("1.2")

multifunc_pkg <- c("multifunc")

# 加载资源包
source("01-LoadPkgs.R")

# 加载函数
source("02-LoadFunc.R")

# 导入原始数据
data <- read.csv("data/Paper3.csv", 
                         sep = ",", header = TRUE, stringsAsFactors = F)

# 所有变量名的集合
Variables <- c("MWD",
               "BD","SOC","TN",
               "pH","acid_p","alka_p",
               "urease","nitro_r","sucrase",
               "SM","Olsen.P","MBC","MBN","respiration")

#---- scale data 通过scale函数对数据进行标准化处理
Zdata <- data
Zdata[ , Variables] <- scale(Zdata[ , Variables])

# 均值法求出我们的功能集合
cluster1 = c("acid_p", "nitro_r")
Zdata$cluster1 = rowMeans(Zdata[, cluster1])

cluster2 = c("BD", "SM")
Zdata$cluster2 = rowMeans(Zdata[, cluster2])

cluster3 = c("Olsen.P", "MBN", "alka_p", "MBC")
Zdata$cluster3 = rowMeans(Zdata[, cluster3])

cluster4 = c("urease", "sucrase", "pH", "respiration", "MWD", "SOC", "TN")
Zdata$cluster4 = rowMeans(Zdata[, cluster4])

# 功能集合
Functions <- c("cluster1",
               "cluster2","cluster3","cluster4")


# new data set that looks at the number of functions greater than or equal to a threshold 
# across a wide range of thresholds (1-99%)
# https://search.r-project.org/CRAN/refmans/multifunc/html/getFuncsMaxed.html
# getFuncsMaxed计算每个实验单元中大于或等于各种阈值的函数数量
thresh <- getFuncsMaxed(Zdata, Functions, threshmin=0.01, threshmax=0.99,
                        prepend=c("P","Cs","Cc", "B"), maxN=1)
thresh$percent <- 100*thresh$thresholds

# CC对比图
ggplot(data=thresh, aes(x=Cc, y=funcMaxed, group=percent)) +
  ylab(expression("Number of Functions" >= Threshold)) +
  xlab("Cc") +
  stat_smooth(aes(color=percent), method = "lm", formula = y ~ poly(x, 2)
              ,level=0.95, size = 0.8, se=F) +
  theme_bw(base_size=14) +
  scale_color_gradient(name="Percent of \nMaximum", low="blue", high="red")

# Cs对比图
ggplot(data=thresh, aes(x=Cs, y=funcMaxed, group=percent)) +
  ylab(expression("Number of Functions" >= Threshold)) +
  xlab("Cs") +
  # poly是拟合函数，我们只需要画折线图，1次拟合就够了
  stat_smooth(aes(color=percent), method = "lm", formula = y ~ poly(x, 1)
              ,level=0.95, size = 0.8, se=F) +
  theme_bw(base_size=14) +
  scale_color_gradient(name="Percent of \nMaximum", low="blue", high="red")

# 全部数据，单作间作的对比图
# plot entire spread of thresholds of alternative systems in comparison to C-IT 
# (conventional intensive tillage system)
threshMean <- thresh %>% 
  group_by(Cs, percent)  %>% 
  dplyr::summarise(n= n(),
                   median   = median(funcMaxed, na.rm = T),
                   sd = sd(funcMaxed,   na.rm = T),
                   mean   = mean(funcMaxed, na.rm = T),
                   ci = qt(1 - (0.05 / 2), n - 1) * (sd/sqrt(n)),
                   se = sd/sqrt(n)) 

fctIdx <- which(names(Zdata) %in% Functions)

#--- get threshold values when C-NT is significantly different to C-IT
pvalues = list()
for (i in 1:99) {
  tryCatch({
    mfuncLinear <- glm(funcMaxed ~ Cs
                       ,data = subset(filter(thresh,Cs %in% c("inter","monoculture")), percent == i)
                       ,family = quasipoisson(link = "identity"))
    Anova(mfuncLinear, test.statistic = "F")
    Pval = Anova(mfuncLinear, test.statistic = "F")["Cs", "Pr(>F)"]
  }, error = function(e){})
  pvalues = list.append(pvalues,Pval)
}

matrix_data = matrix(nrow = 99,ncol = 2, dimnames = list(NULL,c("thresholds","Pvalue")))
matrix_data[,1] = 1:99
matrix_data[,2] = unlist(pvalues)
filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds

# plot: please get x and xend from filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds
# to get significant segment plotted
thresplotCs(threshMean,brewer.pal(9,"Blues")[5]) +
  geom_segment(y=length(fctIdx)+0.5, x=26, yend=length(fctIdx)+0.5, xend=33) +  
  geom_segment(y=length(fctIdx)+0.5, x=43, yend=length(fctIdx)+0.5, xend=74) 


# 筛选出CM数据，开始CM单作间作间的对比
threshCM = subset(thresh, Cc == "CM")
threshCM$Cs[threshCM$Cs == "inter"] = "C/M-intercropping"
threshCM$Cs[threshCM$Cs == "monoculture"] = "C/M-monoculture"
# plot entire spread of thresholds of alternative systems in comparison to C-IT 
# (conventional intensive tillage system)
threshMeanCM <- threshCM %>% 
  group_by(Cs, percent)  %>% 
  dplyr::summarise(n= n(),
                   median   = median(funcMaxed, na.rm = T),
                   sd = sd(funcMaxed,   na.rm = T),
                   mean   = mean(funcMaxed, na.rm = T),
                   ci = qt(1 - (0.05 / 2), n - 1) * (sd/sqrt(n)),
                   se = sd/sqrt(n)) 

fctIdxCM <- which(names(Zdata) %in% Functions)

#--- get threshold values when C-NT is significantly different to C-IT
pvaluesCM = list()
for (i in 1:99) {
  tryCatch({
    mfuncLinearCM <- glm(funcMaxed ~ Cs
                         ,data = subset(filter(threshCM,Cs %in% c("C/M-intercropping","C/M-monoculture")), percent == i)
                         ,family = quasipoisson(link = "identity"))
    Anova(mfuncLinearCM, test.statistic = "F")
    PvalCM = Anova(mfuncLinearCM, test.statistic = "F")["Cs", "Pr(>F)"]
  }, error = function(e){})
  pvaluesCM = list.append(pvaluesCM,PvalCM)
}

matrix_data = matrix(nrow = 99,ncol = 2, dimnames = list(NULL,c("thresholds","Pvalue")))
matrix_data[,1] = 1:99
matrix_data[,2] = unlist(pvaluesCM)
filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds

# plot: please get x and xend from filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds
# to get significant segment plotted
thresplotChangeLegend(threshMeanCM,brewer.pal(9,"Blues")[5], "C/M-intercropping","C/M-monoculture") +
  geom_segment(y=length(fctIdx)+0.5, x=26, yend=length(fctIdx)+0.5, xend=33) +  
  geom_segment(y=length(fctIdx)+0.5, x=43, yend=length(fctIdx)+0.5, xend=74) 



# 筛选出FM数据，开始FM单作间作间的对比
threshFM = subset(thresh, Cc == "FM")
threshFM$Cs[threshFM$Cs == "inter"] = "F/M-intercropping"
threshFM$Cs[threshFM$Cs == "monoculture"] = "F/M-monoculture"
# plot entire spread of thresholds of alternative systems in comparison to C-IT 
# (conventional intensive tillage system)
threshMeanFM <- threshFM %>% 
  group_by(Cs, percent)  %>% 
  dplyr::summarise(n= n(),
                   median   = median(funcMaxed, na.rm = T),
                   sd = sd(funcMaxed,   na.rm = T),
                   mean   = mean(funcMaxed, na.rm = T),
                   ci = qt(1 - (0.05 / 2), n - 1) * (sd/sqrt(n)),
                   se = sd/sqrt(n)) 

fctIdxFM <- which(names(Zdata) %in% Functions)

#--- get threshold values when C-NT is significantly different to C-IT
pvaluesFM = list()

for (i in 1:99) {
  tryCatch({
    mfuncLinearFM <- glm(funcMaxed ~ Cs
                         ,data = subset(filter(threshFM,Cs %in% c("F/M-intercropping","F/M-monoculture")), percent == i)
                         ,family = quasipoisson(link = "identity"))
    Anova(mfuncLinearFM, test.statistic = "F")
    PvalFM = Anova(mfuncLinearFM, test.statistic = "F")["Cs", "Pr(>F)"]
  }, error = function(e){})
  pvaluesFM = list.append(pvaluesFM,PvalFM)
}

matrix_data = matrix(nrow = 99,ncol = 2, dimnames = list(NULL,c("thresholds","Pvalue")))
matrix_data[,1] = 1:99
matrix_data[,2] = unlist(pvaluesFM)
filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds

# plot: please get x and xend from filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds
# to get significant segment plotted
thresplotChangeLegend(threshMeanFM,brewer.pal(9,"Blues")[5], "F/M-intercropping","F/M-monoculture") +
  geom_segment(y=length(fctIdx)+0.5, x=26, yend=length(fctIdx)+0.5, xend=33) +  
  geom_segment(y=length(fctIdx)+0.5, x=43, yend=length(fctIdx)+0.5, xend=74) 




# 筛选出RM数据，开始RM单作间作间的对比
threshRM = subset(thresh, Cc == "RM")
threshRM$Cs[threshRM$Cs == "inter"] = "R/M-intercropping"
threshRM$Cs[threshRM$Cs == "monoculture"] = "R/M-monoculture"
# plot entire spread of thresholds of alternative systems in comparison to C-IT 
# (conventional intensive tillage system)
threshMeanRM <- threshRM %>% 
  group_by(Cs, percent)  %>% 
  dplyr::summarise(n= n(),
                   median   = median(funcMaxed, na.rm = T),
                   sd = sd(funcMaxed,   na.rm = T),
                   mean   = mean(funcMaxed, na.rm = T),
                   ci = qt(1 - (0.05 / 2), n - 1) * (sd/sqrt(n)),
                   se = sd/sqrt(n)) 

fctIdxRM <- which(names(Zdata) %in% Functions)

#--- get threshold values when C-NT is significantly different to C-IT
pvaluesRM = list()
for (i in 1:99) {
  tryCatch({
    mfuncLinearRM <- glm(funcMaxed ~ Cs
                         ,data = subset(filter(threshRM,Cs %in% c("R/M-intercropping","R/M-monoculture")), percent == i)
                         ,family = quasipoisson(link = "identity"))
    Anova(mfuncLinearRM, test.statistic = "F")
    PvalRM = Anova(mfuncLinearRM, test.statistic = "F")["Cs", "Pr(>F)"]
  }, error = function(e){})
  pvaluesRM = list.append(pvaluesRM,PvalRM)
}

matrix_data = matrix(nrow = 99,ncol = 2, dimnames = list(NULL,c("thresholds","Pvalue")))
matrix_data[,1] = 1:99
matrix_data[,2] = unlist(pvaluesRM)
filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds

# plot: please get x and xend from filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds
# to get significant segment plotted
thresplotChangeLegend(threshMeanRM,brewer.pal(9,"Blues")[5], "R/M-intercropping","R/M-monoculture") +
  geom_segment(y=length(fctIdx)+0.5, x=26, yend=length(fctIdx)+0.5, xend=33) +  
  geom_segment(y=length(fctIdx)+0.5, x=43, yend=length(fctIdx)+0.5, xend=74) 



# 筛选出SM数据，开始SM单作间作间的对比
threshSM = subset(thresh, Cc == "SM")
threshSM$Cs[threshSM$Cs == "inter"] = "S/M-intercropping"
threshSM$Cs[threshSM$Cs == "monoculture"] = "S/M-monoculture"
# plot entire spread of thresholds of alternative systems in comparison to C-IT 
# (conventional intensive tillage system)
threshMeanSM <- threshSM %>% 
  group_by(Cs, percent)  %>% 
  dplyr::summarise(n= n(),
                   median   = median(funcMaxed, na.rm = T),
                   sd = sd(funcMaxed,   na.rm = T),
                   mean   = mean(funcMaxed, na.rm = T),
                   ci = qt(1 - (0.05 / 2), n - 1) * (sd/sqrt(n)),
                   se = sd/sqrt(n)) 

fctIdxSM <- which(names(Zdata) %in% Functions)

#--- get threshold values when C-NT is significantly different to C-IT
pvaluesSM = list()
for (i in 1:99) {
  tryCatch({
    mfuncLinearSM <- glm(funcMaxed ~ Cs
                         ,data = subset(filter(threshSM,Cs %in% c("S/M-intercropping","S/M-monoculture")), percent == i)
                         ,family = quasipoisson(link = "identity"))
    Anova(mfuncLinearSM, test.statistic = "F")
    PvalSM = Anova(mfuncLinearSM, test.statistic = "F")["Cs", "Pr(>F)"]
  }, error = function(e){})
  pvaluesSM = list.append(pvaluesSM,PvalSM)
}

matrix_data = matrix(nrow = 99,ncol = 2, dimnames = list(NULL,c("thresholds","Pvalue")))
matrix_data[,1] = 1:99
matrix_data[,2] = unlist(pvaluesSM)
filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds

# plot: please get x and xend from filter(data.frame(matrix_data), Pvalue < 0.05)$thresholds
# to get significant segment plotted
thresplotChangeLegend(threshMeanSM,brewer.pal(9,"Blues")[5], "S/M-intercropping","S/M-monoculture") +
  geom_segment(y=length(fctIdx)+0.5, x=26, yend=length(fctIdx)+0.5, xend=33) +  
  geom_segment(y=length(fctIdx)+0.5, x=43, yend=length(fctIdx)+0.5, xend=74) 

