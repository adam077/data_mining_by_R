data <- read.csv("customers.csv")

#清洗一些空值
table(data$Gender)
table(data$Marital.Status)
data <- data[data$Gender!='NULL' & data$Marital.Status!='NULL',]

data$Gender <- factor(data$Gender)
data$Marital.Status <- factor(data$Marital.Status)

mmFormula <- ~ Home.Ownership + Education.Level + Gender + Marital.Status;
table(data$Home.Ownership)
modelData <- model.matrix(mmFormula, data)
colnames(modelData)

data <- modelData[, -1]
data <- as.data.frame(modelData[, -1])

total <- nrow(data)
index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

data.train.glm = glm(
  formula = Home.OwnershipRent ~ ., 
  data = data.train,
  family='binomial'
)

data.test.predict <- predict(
  data.train.glm, 
  newdata = data.test,
  type="response"
)

data.test.predict <- ifelse(data.test.predict>0.5, "Rent", "Own")

table(data.test$Home.Ownership, data.test.predict)
