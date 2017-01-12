data <- read.csv("customers.csv")
data <- data[data$Gender!='NULL' & data$Marital.Status!='NULL',]
data <- data[, colnames(data)!='CustomerID']

formula <- Home.Ownership ~ .;

total <- nrow(data)
index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

data.train.glm = glm(
  formula = formula, 
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
prop.table(table(data.test$Home.Ownership, data.test.predict), 1)
