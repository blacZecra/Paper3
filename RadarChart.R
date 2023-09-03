install.packages("fmsb")
library(fmsb)
devtools::install_github("ricardo-bion/ggradar")
library("ggradar")

# 导入原始数据
dataOriginal <- read.csv("data/Paper3.csv", 
                              sep = ",", header = TRUE, stringsAsFactors = F)

dataCM <- subset(dataOriginal, Cc == "CM")
dataFM <- subset(dataOriginal, Cc == "FM")
dataRM <- subset(dataOriginal, Cc == "RM")
dataSM <- subset(dataOriginal, Cc == "SM")

Variables <- c("MWD",
               "BD","SOC","TN",
               "pH","酸性磷酸酶","碱性磷酸酶",
               "脲酶活性","硝酸还原酶","蔗糖酶",
               "含水量","Olsen.P","MBC","MBN","respiration")

# 定义变量最大最小值
max_min <- data.frame(
  Cc = c(NA, NA), P = c(NA, NA), Cs = c(NA, NA), B = c(NA, NA),
  MWD = c(1.6, 0.5), BD = c(1.7, 1.4), SOC = c(11, 8),
  TN = c(0.9, 0.6), pH = c(8, 7.5), 酸性磷酸酶 = c(5, 1.5),
  碱性磷酸酶 = c(5.2, 2.5), 脲酶活性 = c(8, 2), 硝酸还原酶 = c(5, 1), 蔗糖酶 = c(12, 5), 
  含水量 = c(0.2, 0.1), Olsen.P = c(30, 0), MBC = c(130, 40), MBN = c(40, 10), respiration = c(1, 0)
)

# 合并数据
max_min$Cc = c("CM", "CM")
max_min$P = c("P0", "P0")
max_min$B = c("BA", "BA")
max_min$Cs = c("na", "na")
#df <- rbind(max_min, dataCM)
testData = subset(dataCM, P == "P0" & B == "BA")[, Variables]
testData = scale(testData)

ggradar(
  testData, 
  values.radar = c("-1.5", "0", "1.5"),
  grid.min = -1.5, grid.mid = 0, grid.max = 1.5,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
)

radarchart(
  testData, axistype = 1,
  # Customize the polygon
  pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = 0.7, vlabels = colnames(testData),
  caxislabels = c(0, 5, 10, 15, 20))
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(testData[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 1.5
)

