data <- read.csv(
  'data.csv', 
  stringsAsFactors=FALSE, 
  fileEncoding='UTF8'
);

knn <- function(data, d, pColumns, tColumn, k=2) {
  #d <-c(3, 7);
  #pColumns <- c('耐酸时间', '压强');
  #tColumn <- c('检验结果');
  #k <- 2
  dist <- apply(data[, pColumns], 1, function(r) {
    return (sqrt(sum((r - d)^2)));
  });
  names(dist) <- data[, tColumn]
  #获取前K个最近的邻居
  result <- sort(dist)[1:k];
  dr <- as.data.frame(table(names(result)), stringsAsFactors=FALSE)
  #统计排序返回出现次数最大的第一个
  return (dr[order(dr$Freq)[1], 1])
}

knn(data, c(3, 7), c('耐酸时间', '压强'), c('检验结果'))
