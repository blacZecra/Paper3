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

# 定义函数P-P图
normPPTest = function(data) {
  m=mean(data)
  sd=sd(data)
  plot((rank(data)-0.5)/length(data), pnorm(data,mean=m, sd=sd),main=('Normal P-P Plot'))
  abline(0,1) # 画y=0+1x直线
}

# 定义函数Q-Q图
normQQTest = function(data) {
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
normPPTest(data)
normQQTest(data)

data <- dataEx1MmS$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1MmS$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1MmS$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# BM self
data <- dataEx1BmS$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmS$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmS$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmS$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# WM self
data <- dataEx1WmS$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmS$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmS$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmS$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# MM common
data <- dataEx1MmC$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1MmC$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1MmC$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1MmC$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# BM common
data <- dataEx1BmC$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmC$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmC$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1BmC$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# WM common
data <- dataEx1WmC$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmC$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmC$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx1WmC$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# ALL
data <- dataExperiment1$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment1$root.biomass.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment1$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment1$Specific.root.length..m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)


########################## 实验2:
dataEx2SM = subset(dataExperiment2, culture == "soil" & treatment == "M")
dataEx2SMW_M = subset(dataExperiment2, culture == "soil" & treatment == "M/W-M")
dataEx2SW = subset(dataExperiment2, culture == "soil" & treatment == "W")
dataEx2SMW_W = subset(dataExperiment2, culture == "soil" & treatment == "M/W-W")
dataEx2QSM = subset(dataExperiment2, culture == "quartz sand" & treatment == "M")
dataEx2QSMW_M = subset(dataExperiment2, culture == "quartz sand" & treatment == "M/W-M")
dataEx2QSW = subset(dataExperiment2, culture == "quartz sand" & treatment == "W")
dataEx2QSMW_W = subset(dataExperiment2, culture == "quartz sand" & treatment == "M/W-W")
dataEx2SAll = subset(dataExperiment2, culture == "soil")
dataEx2S_M = subset(dataExperiment2, culture == "soil" & (treatment == "M" | treatment == "M/W-M"))
dataEx2S_W = subset(dataExperiment2, culture == "soil" & (treatment == "W" | treatment == "M/W-W"))
dataEx2QAll = subset(dataExperiment2, culture == "quartz sand")
dataEx2Q_M = subset(dataExperiment2, culture == "quartz sand" & (treatment == "M" | treatment == "M/W-M"))
dataEx2Q_W = subset(dataExperiment2, culture == "quartz sand" & (treatment == "W" | treatment == "M/W-W"))

# soil M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2SM[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# soil MW-M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2SMW_M[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# soil W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2SW[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}


# soil MW-W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2SMW_W[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2QSM[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand MW-M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2QSMW_M[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2QSW[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand MW-W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2QSMW_W[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# soil all
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2SAll[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# soil all M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2S_M[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# soil all W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2S_W[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand all
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2QAll[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand all M
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2Q_M[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}

# quartz sand all W
for(i in colnames(dataExperiment2[, 3:ncol(dataExperiment2)])) {
  # 跳过正态检验列
  if (i == "正态检验") {
    next
  }
  print(i)
  data <- dataEx2Q_W[,i]
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}


########################## 实验3:
dataEx3_M = subset(dataExperiment3, Treatments == "Maize grown alone")
dataEx3_MW = subset(dataExperiment3, Treatments == "Maize grown with wheat plant")
dataEx3_MW_Ex = subset(dataExperiment3, Treatments == "Maize grown with wheat root exudates")
# Maize grown alone 
data <- dataEx3_M$root.length..m
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_M$Root.biomass..10.2.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_M$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_M$Specific.root.length..m.g.1.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# Maize grown with wheat plant
data <- dataEx3_MW$root.length..m
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW$Root.biomass..10.2.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW$Specific.root.length..m.g.1.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# Maize grown with wheat root exudates
data <- dataEx3_MW_Ex$root.length..m
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW_Ex$Root.biomass..10.2.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW_Ex$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx3_MW_Ex$Specific.root.length..m.g.1.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# All
data <- dataExperiment3$root.length..m
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment3$Root.biomass..10.2.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment3$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment3$Specific.root.length..m.g.1.
shapiro.test(data)
normPPTest(data)
normQQTest(data)


########################## 实验4:
dataEx4_CK = subset(dataExperiment4, treat == "CK")
dataEx4_maize = subset(dataExperiment4, treat == "maize")
dataEx4_barley = subset(dataExperiment4, treat == "barley")
dataEx4_wheat = subset(dataExperiment4, treat == "wheat")
# CK 
# root length
data <- dataEx4_CK$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.biomass
data <- dataEx4_CK$Root.biomass
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.surface.area
data <- dataEx4_CK$Root.surface.area
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Specific.root.length
data <- dataEx4_CK$Specific.root.length
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# maize 
# root length
data <- dataEx4_maize$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.biomass
data <- dataEx4_maize$Root.biomass
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.surface.area
data <- dataEx4_maize$Root.surface.area
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Specific.root.length
data <- dataEx4_maize$Specific.root.length
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# barley 
# root length
data <- dataEx4_barley$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.biomass
data <- dataEx4_barley$Root.biomass
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.surface.area
data <- dataEx4_barley$Root.surface.area
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Specific.root.length
data <- dataEx4_barley$Specific.root.length
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# wheat 
# root length
data <- dataEx4_wheat$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.biomass
data <- dataEx4_wheat$Root.biomass
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.surface.area
data <- dataEx4_wheat$Root.surface.area
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Specific.root.length
data <- dataEx4_wheat$Specific.root.length
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# All
# root length
data <- dataExperiment4$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.biomass
data <- dataExperiment4$Root.biomass
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Root.surface.area
data <- dataExperiment4$Root.surface.area
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# Specific.root.length
data <- dataExperiment4$Specific.root.length
shapiro.test(data)
normPPTest(data)
normQQTest(data)

########################## 实验5:
dataEx5_WW = subset(dataExperiment5, treatment == "WW")
dataEx5_MM = subset(dataExperiment5, treatment == "MM")
dataEx5_WpM = subset(dataExperiment5, treatment == "W+M")
dataEx5_WM = subset(dataExperiment5, treatment == "WM")
# WW
data <- dataEx5_WW$MBOA.concentration..μg.pot.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# MM
data <- dataEx5_MM$MBOA.concentration..μg.pot.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# W+M
data <- dataEx5_WpM$MBOA.concentration..μg.pot.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# WM
data <- dataEx5_WM$MBOA.concentration..μg.pot.
shapiro.test(data)
normPPTest(data)
normQQTest(data)
# All
data <- dataExperiment5$MBOA.concentration..μg.pot.
shapiro.test(data)
normPPTest(data)
normQQTest(data)


########################## 实验6:
dataEx6_maize = subset(dataExperiment6, crop == "maize")
dataEx6_wheat = subset(dataExperiment6, crop == "wheat")
# maize
data <- dataEx6_maize$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_maize$Root.biomass..10.1g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_maize$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_maize$Specific.root.length.m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# wheat
data <- dataEx6_wheat$root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_wheat$Root.biomass..10.1g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_wheat$Root.surface.area..cm2.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx6_wheat$Specific.root.length.m.g.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

########################## 实验7:
dataEx7_dose0 = subset(dataExperiment7, dose == 0)
dataEx7_dose0_3 = subset(dataExperiment7, dose == 0.3)
# dose == 0
data <- dataEx7_dose0$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx7_dose0$primary.root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx7_dose0$seminal.root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# dose == 0.3
data <- dataEx7_dose0_3$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx7_dose0_3$primary.root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataEx7_dose0_3$seminal.root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# ALL
data <- dataExperiment7$root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment7$primary.root.length..m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)

data <- dataExperiment7$seminal.root.length.m.
shapiro.test(data)
normPPTest(data)
normQQTest(data)



########################## 实验8:
dataEx8_P_PIN2 = subset(dataExperiment8, root == "primary" & gene == "PIN2")
dataEx8_P_IAA2 = subset(dataExperiment8, root == "primary" & gene == "IAA2")
dataEx8_P_IAA21 = subset(dataExperiment8, root == "primary" & gene == "IAA21")
dataEx8_P_ARF2 = subset(dataExperiment8, root == "primary" & gene == "ARF2")
dataEx8_P_GH3 = subset(dataExperiment8, root == "primary" & gene == "GH3")

dataEx8_S_PIN2 = subset(dataExperiment8, root == "seminal" & gene == "PIN2")
dataEx8_S_IAA2 = subset(dataExperiment8, root == "seminal" & gene == "IAA2")
dataEx8_S_IAA21 = subset(dataExperiment8, root == "seminal" & gene == "IAA21")
dataEx8_S_ARF2 = subset(dataExperiment8, root == "seminal" & gene == "ARF2")
dataEx8_S_GH3 = subset(dataExperiment8, root == "seminal" & gene == "GH3")
# primary PIN2
data <- dataEx8_P_PIN2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# primary IAA2
data <- dataEx8_P_IAA2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# primary IAA21
data <- dataEx8_P_IAA21$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# primary ARF2
data <- dataEx8_P_ARF2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# primary GH3
data <- dataEx8_P_GH3$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# seminal PIN2
data <- dataEx8_S_PIN2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# seminal IAA2
data <- dataEx8_S_IAA2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# seminal IAA21
data <- dataEx8_S_IAA21$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# seminal ARF2
data <- dataEx8_S_ARF2$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

# seminal GH3
data <- dataEx8_S_GH3$expression
shapiro.test(data)
normPPTest(data)
normQQTest(data)

time = c(6, 12, 24, 48, 96, 192)
for (t in time) {
  print(t)
  dataEx8_P_PIN2 = subset(dataExperiment8, root == "primary" & gene == "PIN2" & time == t)
  dataEx8_P_IAA2 = subset(dataExperiment8, root == "primary" & gene == "IAA2" & time == t)
  dataEx8_P_IAA21 = subset(dataExperiment8, root == "primary" & gene == "IAA21" & time == t)
  dataEx8_P_ARF2 = subset(dataExperiment8, root == "primary" & gene == "ARF2" & time == t)
  dataEx8_P_GH3 = subset(dataExperiment8, root == "primary" & gene == "GH3" & time == t)
  
  dataEx8_S_PIN2 = subset(dataExperiment8, root == "seminal" & gene == "PIN2" & time == t)
  dataEx8_S_IAA2 = subset(dataExperiment8, root == "seminal" & gene == "IAA2" & time == t)
  dataEx8_S_IAA21 = subset(dataExperiment8, root == "seminal" & gene == "IAA21" & time == t)
  dataEx8_S_ARF2 = subset(dataExperiment8, root == "seminal" & gene == "ARF2" & time == t)
  dataEx8_S_GH3 = subset(dataExperiment8, root == "seminal" & gene == "GH3" & time == t)
  
  # primary PIN2
  print("primary PIN2:")
  data <- dataEx8_P_PIN2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # primary IAA2
  print("primary IAA2:")
  data <- dataEx8_P_IAA2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # primary IAA21
  print("primary IAA21:")
  data <- dataEx8_P_IAA21$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # primary ARF2
  print("primary ARF2:")
  data <- dataEx8_P_ARF2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # primary GH3
  print("primary GH3:")
  data <- dataEx8_P_GH3$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # seminal PIN2
  print("seminal PIN2:")
  data <- dataEx8_S_PIN2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # seminal IAA2
  print("seminal IAA2:")
  data <- dataEx8_S_IAA2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # seminal IAA21
  print("seminal IAA21:")
  data <- dataEx8_S_IAA21$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # seminal ARF2
  print("seminal ARF2:")
  data <- dataEx8_S_ARF2$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
  
  # seminal GH3
  print("seminal GH3:")
  data <- dataEx8_S_GH3$expression
  swTest = shapiro.test(data)
  print(swTest)
  normPPTest(data)
  normQQTest(data)
}
