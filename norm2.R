# 导入原始数据
dataOriginal <- read.csv("data/norm.csv", 
                         sep = ",", header = TRUE, stringsAsFactors = F)
dataExperiment1 <- read.csv("data/experiment1.csv", 
                         sep = ",", header = TRUE, stringsAsFactors = F)

data <- dataOriginal$根长.cm.
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

data <- dataOriginal$根表面积.cm2.
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

data <- dataOriginal$根系平均直径.mm.
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

data <- dataOriginal$根体积.cm3.
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

data <- dataOriginal$地上部干重
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

dataEx1MmS = subset(dataExperiment1, root.interaction == "MM" & area == "common")
dataEx1BmS = subset(dataExperiment1, root.interaction == "BM" & area == "common")
dataEx1WmS = subset(dataExperiment1, root.interaction == "WM" & area == "common")
dataEx1MmS = subset(dataExperiment1, root.interaction == "MM" & area == "common")
dataEx1BmS = subset(dataExperiment1, root.interaction == "BM" & area == "common")
dataEx1WmS = subset(dataExperiment1, root.interaction == "WM" & area == "common")

data <- dataEx1MmS$rootLength
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

data <- dataEx1BmS$rootLength
m=mean(data)
sd=sd(data)
shapiro.test(data) # 使用数据与均数方差相等的正态分布比较
plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
abline(0,1) # 画y=0+1x直线
qqnorm(data)
qqline(data)

