library(nnet)

data <- read.csv("customers.csv");
data <- data[
    data$Gender!='NULL' & 
    data$Marital.Status!='NULL' &
    data$Home.Ownership!='NULL' &
    data$Education.Level!='NULL',
  ];

data$Gender <- factor(data$Gender)
data$Marital.Status <- factor(data$Marital.Status)
data$Home.Ownership <- factor(data$Home.Ownership)
data$Education.Level <- factor(data$Education.Level)

mmFormula <- ~ Home.Ownership + Education.Level + Gender + Marital.Status;

modelData <- model.matrix(mmFormula, data)
colnames(modelData)

data <- as.data.frame(modelData[, -1])

total <- nrow(data)
index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

names(data)

data.train.nnet = nnet(
  formula = Home.OwnershipRent ~ ., 
  data = data.train,
  size = 10,
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
