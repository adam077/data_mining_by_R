#install.packages('TTR')
library(TTR)

data <- read.csv("data1.csv", fileEncoding="UTF8")

plot(data$公司A, type='l')

data$SMA <- SMA(data$公司A, n=3)

lines(data$SMA)



plot(data$公司A, type='l')

data$WMA <- WMA(data$公司A, n=3, wts=1:3)

lines(data$WMA)
