# 设置工作路径
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(NbClust)

# 导入原始数据
dataOriginal <- read.csv("data/Paper3.csv", 
                         sep = ",", header = TRUE, stringsAsFactors = F)
Variables <- c("MWD",
               "BD","SOC","TN",
               "pH","acid_p","alka_p",
               "urease","nitro_r","sucrase",
               "SM","Olsen.P","MBC","MBN","respiration")

clusterDataOriginal = data.matrix(t(dataOriginal[,Variables]))
# 归一化
dataOriginal[,Variables] = scale(dataOriginal[,Variables])
# 矩阵旋转

clusterData = data.matrix(t(dataOriginal[,Variables]))

d <- dist(clusterData)
fit.average <- hclust(d, method = "average")

plot(fit.average, hang = -1, cex = .8, main = "Average Linkage Clustering")


# step2: 选择聚类个数
# install.packages("mclust")
library(mclust)
m_clust <- Mclust(clusterData, G = 3:9)
summary(m_clust)
plot(m_clust, "BIC")

devAskNewPage(ask = TRUE)
nc <- NbClust(clusterData, distance = "euclidean", min.nc = 2,
              max.nc = 15, method = "average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab = "Number of Clusters", 
        ylab = "Number of Criteria", main = "Number of Clusters Chosen")

#step3: 聚类结果
clusters <- cutree(fit.average, k = 4)
table(clusters)
# 描述聚类
aggregate(clusterDataOriginal, by = list(cluster = clusters), median)
aggregate(as.data.frame(clusterData), by = list(cluster = clusters), median)

# 结果绘图
plot(fit.average, hang = -1, cex=.8, main = "Average Linkage Clustering\n4 Cluster Solution")
rect.hclust(fit.average, k = 4)

