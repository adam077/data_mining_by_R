data <- read.csv("data2.csv", fileEncoding = "UTF8")

freq <- spec.pgram(data$总销量, taper=0, log='no', plot=FALSE);

start <- which(freq$spec==max(freq$spec))
frequency <- 1/freq$freq[which(freq$spec==max(freq$spec))]


data$均值 <- data$总销量/data$分店数

freq <- spec.pgram(data$均值, taper=0, log='no', plot=FALSE);

start <- which(freq$spec==max(freq$spec))
frequency <- 1/freq$freq[which(freq$spec==max(freq$spec))]

plot(data$均值, type='l')

meanTS <- ts(
  data$均值[start:length(data$均值)], 
  frequency=frequency
) 
ts.plot(meanTS)

meanTSdecompose <- decompose(meanTS)
plot(meanTSdecompose)

#趋势分解
meanTSdecompose$trend
#季节性分解数据
meanTSdecompose$seasonal
#随机部分
meanTSdecompose$random
