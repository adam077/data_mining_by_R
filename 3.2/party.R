#install.packages("party")
library(party)

data <- read.csv("data.csv")

formula <- CollegePlans ~ Gender+ParentIncome+IQ+ParentEncouragement
#CollegePlans ~ .

CollegePlansTree <- ctree(formula, data=data)
plot(CollegePlansTree)
plot(CollegePlansTree, type="simple")


subData <- data[data$ParentIncome>67410 & data$ParentEncouragement=='Encouraged', ]
prop.table(table(subData$CollegePlans))

#交叉验证
total <- nrow(data)

index <- sample(1:total, total*0.7)

data.train <- data[index, ]
data.test <- data[-index, ]

CollegePlansTree <- ctree(formula, data=data.train)

data.test.predict <- predict(CollegePlansTree, newdata=data.test)

prop.table(table(data.test$CollegePlans, data.test.predict), 1)
