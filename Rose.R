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

# scale Variables, Proxies and Functions between 0 and 1 to ease readability
for (i in colnames(Zdata[,5:ncol(Zdata)])) {
  Zdata[,i] = Zdata[,i] + abs(min(Zdata[,i], na.rm = T))
  Zdata[,i] = Zdata[,i] / max(Zdata[,i], na.rm = T)
}

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


# arrange data for plotting
Pdata <- Zdata[,c("Cc","Cs",Variables)] %>% 
  reshape2::melt(id.vars = c("Cc","Cs"))

# add service categories to data
Pdata$services <- NA
Pdata$services[which(Pdata$variable=="acid_p" | Pdata$variable=="nitro_r")
] <- "cluster1"

Pdata$services[which(Pdata$variable=="BD" | Pdata$variable=="SM")
] <- "cluster2"

Pdata$services[which(Pdata$variable=="Olsen.P" | Pdata$variable=="MBN" | 
                       Pdata$variable=="alka_p" | Pdata$variable=="MBC")
] <- "cluster3"

Pdata$services[which(Pdata$variable=="urease" | Pdata$variable=="sucrase" | 
                       Pdata$variable=="pH" | Pdata$variable=="respiration" |
                       Pdata$variable=="MWD" | Pdata$variable=="SOC" | Pdata$variable=="TN")
] <- "cluster4"

Pdata$services <- factor(Pdata$services, 
                         levels = c("cluster1","cluster2","cluster3","cluster4"))

Pdata$variable <- factor(Pdata$variable, 
                         levels = c("MWD",
                                    "BD","SOC","TN",
                                    "pH","acid_p","alka_p",
                                    "urease","nitro_r","sucrase",
                                    "SM","Olsen.P","MBC","MBN","respiration"))
# summarize data 
summary <- Pdata  %>% group_by(Cc, services, variable) %>%
  dplyr::summarize(n = n(),
                   mean = mean(value, na.rm = T),
                   sd = sd(value, na.rm = T),
                   ci = qt(1 - (0.1 / 2), n - 1) * (sd/sqrt(n)))


#--- see R script 02-LoadFunc.R to see plot code
# plot data
grid.arrange(
  Plot.circleCc("CM"),
  Plot.circleCc("FM"),
  Plot.circleCc("RM"),
  Plot.circleCc("SM"),
  ncol = 2, nrow = 2)


# summarize data 
summary <- Pdata  %>% group_by(Cs, services, variable) %>%
  dplyr::summarize(n = n(),
                   mean = mean(value, na.rm = T),
                   sd = sd(value, na.rm = T),
                   ci = qt(1 - (0.1 / 2), n - 1) * (sd/sqrt(n)))


#--- see R script 02-LoadFunc.R to see plot code
# plot data
grid.arrange(
  Plot.circleCs("inter"),
  Plot.circleCs("monoculture"),
  ncol = 2, nrow = 2)
