data <- read.csv("data.csv", fileEncoding="UTF8")
#data <- read.csv("data.csv", fileEncoding="UTF8", stringsAsFactors=FALSE)

datas <- split(data, data$广告商)

plot(
  datas[[2]]$销售量
)

datas[[2]]$month <- 1:12

data.model <- nls(
  销售量 ~ A*month + B, 
  start=list(A=1, B=1), 
  data=datas[[2]], 
  trace=T
)

lines(
  datas[[2]]$month, 
  8.248*datas[[2]]$month + 345.970 
)

data.model <- nls(
  销售量 ~ A^month, 
  start=list(A=1), 
  data=datas[[2]], 
  trace=T
)

lines(
  datas[[2]]$month, 
  1.713^datas[[2]]$month
)


data.model <- nls(
  销售量 ~ A*month^2 + B*month + C, 
  start=list(A=0, B=8, C=300), 
  data=datas[[2]], 
  trace=T
)

lines(
  datas[[2]]$month, 
  -0.02972*datas[[2]]$month^2 + 8.63462*datas[[2]]$month + 345.06818 
)

