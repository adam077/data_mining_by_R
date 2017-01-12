#install.packages('fpc')

library('fpc')

data <- read.csv('data.csv')
plot(data[, 1], data[, 2])

# 用K均值聚类
model1 <- kmeans(data, centers=2)
plot(data[, 1], data[, 2], col=model1$cluster)

# 用fpc包中的dbscan函数进行密度聚类
model2 <- dbscan(data, eps=0.2, MinPts=5)
plot(data[, 1], data[, 2], col=model2$cluster)
