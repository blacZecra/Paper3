
#install.packages("sysfonts")
#install.packages("showtextdb")
#install.packages("showtext")
#library(sysfonts)
#library(showtextdb)
#library(showtext)

install.packages("fmsb")
library(fmsb)
devtools::install_github("ricardo-bion/ggradar")
library("ggradar")
library(ggplot2)

# 设置工作路径
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 导入原始数据
dataOriginal <- read.csv("data/Paper3.csv", 
                              sep = ",", header = TRUE, stringsAsFactors = F)

Variables <- c("MWD",
               "BD","SOC","TN",
               "pH","acid_p","alka_p",
               "urease","nitro_r","sucrase",
               "SM","Olsen.P","MBC","MBN","respiration")
# 归一化
dataOriginal[,Variables] = scale(dataOriginal[,Variables])

# Cc分组取值
dataCM <- subset(dataOriginal, Cc == "CM")
dataFM <- subset(dataOriginal, Cc == "FM")
dataRM <- subset(dataOriginal, Cc == "RM")
dataSM <- subset(dataOriginal, Cc == "SM")

testData = subset(dataCM, P == "P0" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "CM-P0"
) + 
theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataCM, P == "P40" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "CM-P40"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataCM, P == "P80" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "CM-P80"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))




#FM
testData = subset(dataFM, P == "P0" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "FM-P0"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataFM, P == "P40" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "FM-P40"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataFM, P == "P80" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "FM-P80"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))





#RM
testData = subset(dataRM, P == "P0" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "RM-P0"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataRM, P == "P40" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "RM-P40"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataRM, P == "P80" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "RM-P80"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))





#SM
testData = subset(dataSM, P == "P0" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "SM-P0"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataSM, P == "P40" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "SM-P40"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

testData = subset(dataSM, P == "P80" & B == "BA")[, Variables]
legend = c("inter", "monoculture")
data = data.frame(legend, testData)
ggradar(
  data, 
  font.radar = "Arial Unicode MS",
  values.radar = c("-2.5", "0", "2.5"),
  grid.min = -2.5, grid.mid = 0, grid.max = 2.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  plot.title = "SM-P80"
) + 
  theme(plot.title = element_text(hjust = 0.5,size = 16))


