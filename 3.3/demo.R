#install.packages("klaR")
library(klaR)

data <- read.csv("data.csv")

total <- nrow(data)

index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

formula=CollegePlans ~ Gender + ParentIncome + IQ + ParentEncouragement
model <- NaiveBayes(formula, data.train)
data.test.predict <- predict(model, newdata = data.test)

prop.table(table(data.test$CollegePlans, data.test.predict$class), 1)
