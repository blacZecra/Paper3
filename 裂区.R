setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(agricolae)
library(readxl)
data <- read_excel("data/2023bing.xlsx", sheet = "wuweibing")
data$BLOCK <- as.character(data$BLOCK)
data$CS<- as.character(data$CS)
data$N<- as.character(data$N)
head(data)
dim(data)

fit<-aov(YNU ~ N*CS + Error(BLOCK/N) ,data=data)
summary(fit)

with(data,LSD.test(YNU,N,DFerror=3,MSerror= 269,console=T))#change the value in Error:within-Residuals
with(data,LSD.test(YNU,CS,DFerror=2,MSerror=293,console=T))#change the value in Error:within-Residuals
with(data,LSD.test(YNU,N:CS,DFerror=3,MSerror=340,console=T))#change the value in Error:within-Residuals
