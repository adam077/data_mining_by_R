library(nnet) 

data <- read.csv("customers.csv")
for(colName in names(data)) {
  data <- data[data[, colName]!='NULL', ]
}

write.csv(data, file="data.csv", row.names=FALSE, quote=FALSE)
data <- read.csv("data.csv")

data <- data[, colnames(data)!='CustomerID']

mmFormula <- ~ .;
modelData <- model.matrix(mmFormula, data)
data <- as.data.frame(modelData[, -1])
names(data)

total <- nrow(data)
index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

formula <- Home.OwnershipRent ~ .;
data.train.nnet = nnet(
  formula = formula, 
  data = data.train,
  size = 3,
  decay = 0.1, 
  linout = T, 
  trace = F
)

data.test.predict <- predict(
  data.train.nnet, 
  newdata = data.test
)

data.test.predict <- ifelse(data.test.predict>0.5, "Rent", "Own")

table(data.test$Home.Ownership, data.test.predict)
prop.table(table(data.test$Home.Ownership, data.test.predict), 1)
