data <- read.csv('data.csv')

plot(data[, 1], data[, 2])

eps <- 0.2;
MinPts <- 5;

d <- as.matrix(dist(data))

#将所有点标记为核心点、边界点或噪声点
ps <- data.frame(index=c(NA), density=c(NA), type=c(NA))
for(i in 1:nrow(data)) {
  #i <- 1;
  index <- which(d[i, ]<eps)
  #密度,空间中任意一点的密度是以该点为圆心、以 Eps 为半径的圆区域内包含的点数
  density <- length(index);
  if(density>MinPts) {
    #核心点（Core Points）
    #空间中某一点的密度，如果大于某一给定阈值MinPts，则称该为核心点
    ps[i, ] <- c(i, density, 1)
  } else if(density>1) {
    #边界点（Border Points）
    #空间中某一点的密度，如果小于某一给定阈值MinPts，则称该为边界点
    ps[i, ] <- c(i, density, 2)
  } else {
    #噪声点（Noise Points）
    #数据集中不属于核心点，也不属于边界点的点，也就是密度值为1的点
    ps[i, ] <- c(i, density, 0)
  }
}

#把噪声点过滤掉，因为噪声点无法聚类，它们独自一类
corePoints <- data[which(ps$type!=0), ]
coreDists <- as.matrix(dist(corePoints))

#首先，把每个点的领域都作为一类
#邻域（Neighborhood）
#空间中任意一点的邻域是以该点为圆心、以 Eps 为半径的圆区域内包含的点集合
cluster <- list();
for(i in 1:nrow(coreDists)) {
  cluster[[i]] <- names(which(coreDists[i, ]<eps));
}

#然后，将有交集的领域，都合并为新的领域
for(i in 1:length(cluster)) {
  for(j in 1:length(cluster)) {
    if(any(cluster[[j]] %in% cluster[[i]]) && i!=j) {
      if(ps[cluster[[i]][1], ]$type==1 && ps[cluster[[i]][2], ]$type==1) {
        cluster[[i]] <- unique(append(cluster[[i]], cluster[[j]]))
        cluster[[j]] <- list();
      }
    }
  }
}

#最后，找出独立（也就是没有交集）的领域，就是我们最后的聚类的结果了
result <- list();
for(i in 1:length(cluster)) {
  if(length(cluster[[i]])>0) {
    result[[length(result)+1]] <- cluster[[i]]
  }
}

#找出每个点所在领域的序号，作为他们最后聚类的结果标记
for(i in 1:length(result)) {
  for(j in result[[i]]) {
    data[j, 3] <- i
  }
}

plot(data[, 1], data[, 2], col=data[,3])
