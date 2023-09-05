# 设置工作路径
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 导入原始数据
dataExperiment1 <- read.csv("data/experiment1.csv", 
                         sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment2 <- read.csv("data/experiment2.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment3 <- read.csv("data/experiment3.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment4 <- read.csv("data/experiment4.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment5 <- read.csv("data/experiment5.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment6 <- read.csv("data/experiment6.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment7 <- read.csv("data/experiment7.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment8 <- read.csv("data/experiment8.csv", 
                            sep = ",", header = TRUE, stringsAsFactors = F)

# 定义函数
normTest = function(data) {
  m=mean(data)
  sd=sd(data)
  shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
  plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
  abline(0,1) # 画y=0+1x直线
  qqnorm(data)
  qqline(data)
}

########################## 实验1:
dataEx1MmS = subset(dataExperiment1, root.interaction == "MM" & area == "self")
dataEx1BmS = subset(dataExperiment1, root.interaction == "BM" & area == "self")
dataEx1WmS = subset(dataExperiment1, root.interaction == "WM" & area == "self")
dataEx1MmC = subset(dataExperiment1, root.interaction == "MM" & area == "common")
dataEx1BmC = subset(dataExperiment1, root.interaction == "BM" & area == "common")
dataEx1WmC = subset(dataExperiment1, root.interaction == "WM" & area == "common")
# MM self
data <- dataEx1MmS$root.length.m.
shapiro.test(data)
# normTest(data)

# BM self
data <- dataEx1BmS$root.length.m.
shapiro.test(data)
# WM self
data <- dataEx1WmS$root.length.m.
shapiro.test(data)
# MM common
data <- dataEx1MmC$root.length.m.
shapiro.test(data)
# BM common
data <- dataEx1BmC$root.length.m.
shapiro.test(data)
# WM common
data <- dataEx1WmC$root.length.m.
shapiro.test(data)
# ALL
data <- dataExperiment1$root.length.m.
shapiro.test(data)
normTest(data)


########################## 实验2:
dataEx2SM = subset(dataExperiment2, culture == "soil" & treatment == "M")
dataEx2SMW_M = subset(dataExperiment2, culture == "soil" & treatment == "M/W-M")
dataEx2SW = subset(dataExperiment2, culture == "soil" & treatment == "W")
dataEx2SMW_W = subset(dataExperiment2, culture == "soil" & treatment == "M/W-W")
dataEx2QSM = subset(dataExperiment2, culture == "quartz sand" & treatment == "M")
dataEx2QSMW_M = subset(dataExperiment2, culture == "quartz sand" & treatment == "M/W-M")
dataEx2QSW = subset(dataExperiment2, culture == "quartz sand" & treatment == "W")
dataEx2QSMW_W = subset(dataExperiment2, culture == "quartz sand" & treatment == "M/W-W")
# soil M
data <- dataEx2SM$root.length..m.
shapiro.test(data)
# soil MW-M
data <- dataEx2SMW_M$root.length..m.
shapiro.test(data)
# soil W
data <- dataEx2SW$root.length..m.
shapiro.test(data)
# soil MW-W
data <- dataEx2SMW_W$root.length..m.
shapiro.test(data)
# quartz sand M
data <- dataEx2QSM$root.length..m.
shapiro.test(data)
# quartz sand MW-M
data <- dataEx2QSMW_M$root.length..m.
shapiro.test(data)
# quartz sand W
data <- dataEx2QSW$root.length..m.
shapiro.test(data)
# quartz sand MW-W
data <- dataEx2QSMW_W$root.length..m.
shapiro.test(data)


########################## 实验4:
dataEx4_CK = subset(dataExperiment4, treat == "CK")
dataEx4_maize = subset(dataExperiment4, treat == "maize")
dataEx4_barley = subset(dataExperiment4, treat == "barley")
dataEx4_wheat = subset(dataExperiment4, treat == "wheat")
# CK 
# root length
data <- dataEx4_CK$root.length..m.
shapiro.test(data)
# Root.biomass
data <- dataEx4_CK$Root.biomass
shapiro.test(data)
# Root.surface.area
data <- dataEx4_CK$Root.surface.area
shapiro.test(data)
# Specific.root.length
data <- dataEx4_CK$Specific.root.length
shapiro.test(data)

# maize 
# root length
data <- dataEx4_maize$root.length..m.
shapiro.test(data)
# Root.biomass
data <- dataEx4_maize$Root.biomass
shapiro.test(data)
# Root.surface.area
data <- dataEx4_maize$Root.surface.area
shapiro.test(data)
# Specific.root.length
data <- dataEx4_maize$Specific.root.length
shapiro.test(data)

# barley 
# root length
data <- dataEx4_barley$root.length..m.
shapiro.test(data)
# Root.biomass
data <- dataEx4_barley$Root.biomass
shapiro.test(data)
# Root.surface.area
data <- dataEx4_barley$Root.surface.area
shapiro.test(data)
# Specific.root.length
data <- dataEx4_barley$Specific.root.length
shapiro.test(data)

# wheat 
# root length
data <- dataEx4_wheat$root.length..m.
shapiro.test(data)
# Root.biomass
data <- dataEx4_wheat$Root.biomass
shapiro.test(data)
# Root.surface.area
data <- dataEx4_wheat$Root.surface.area
shapiro.test(data)
# Specific.root.length
data <- dataEx4_wheat$Specific.root.length
shapiro.test(data)

# All Specific.root.length
data <- dataExperiment4$Specific.root.length
shapiro.test(data)
normTest(data)

########################## 实验5:
dataEx5_WW = subset(dataExperiment5, treatment == "WW")
dataEx5_MM = subset(dataExperiment5, treatment == "MM")
dataEx5_WpM = subset(dataExperiment5, treatment == "W+M")
dataEx5_WM = subset(dataExperiment5, treatment == "WM")
# WW
data <- dataEx5_WW$MBOA.concentration..μg.pot.
shapiro.test(data)
# MM
data <- dataEx5_MM$MBOA.concentration..μg.pot.
shapiro.test(data)
# W+M
data <- dataEx5_WpM$MBOA.concentration..μg.pot.
shapiro.test(data)
# WM
data <- dataEx5_WM$MBOA.concentration..μg.pot.
shapiro.test(data)

