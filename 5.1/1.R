data <- read.transactions(
  "data.csv", 
  format = "single", 
  cols = c(1,2), 
  sep=',', 
  skip=1
)
inspect(data)

data2 <- read.transactions(
  "data2.txt", 
  format="basket", 
  sep=",", 
  skip = 1
)
inspect(data2)

rules <- apriori(data)
inspect(rules)

rules <- apriori(
  data, 
  parameter=list(
    support=0.5, 
    confidence=0.5
  )
)
inspect(rules)
