data <- read.csv("data.csv", fileEncoding="UTF8")
#data <- read.csv("data.csv", fileEncoding="UTF8", stringsAsFactors=FALSE)

datas <- split(data, data$广告商)

par(mfrow=c(1, 1))

plot(
  data$销售量
)

datas[[1]]$month <- 1:12

data.model <- nls(
  销售量 ~ A*month + B, 
  start=list(A=1, B=1), 
  data=datas[[1]], 
  trace=T
)

lines(
  datas[[1]]$month, 
  17.36*datas[[1]]$month + 174.14
)

data.model <- nls(
  销售量 ~ A^month, 
  start=list(A=1), 
  data=datas[[1]], 
  trace=T
)

lines(
  datas[[1]]$month, 
  1.685^datas[[1]]$month
)


data.model <- nls(
  销售量 ~ A*month^2 + B*month + C, 
  start=list(A=1, B=1, C=1), 
  data=datas[[1]], 
  trace=T
)

lines(
  datas[[1]]$month, 
  -0.1469*datas[[1]]$month^2 + 19.2727*datas[[1]]$month + 169.6818 
)

