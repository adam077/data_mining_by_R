#install.packages('forecast')
library(forecast)

data <- read.csv("data.csv", fileEncoding="UTF8")
data$均值 <- data$总销量/data$分店数

plot(data$均值, type='l')

freq <- spec.pgram(data$均值, taper=0, log='no', plot=FALSE);

start <- which(freq$spec==max(freq$spec))
frequency <- 1/freq$freq[which(freq$spec==max(freq$spec))]

meanTS <- ts(
  data$均值[start:length(data$均值)], 
  frequency=frequency
)

meanARIMA = auto.arima(meanTS)
meanARIMAForecast = forecast(meanARIMA, h=7);
meanARIMAForecast$mean

plot(meanARIMAForecast)