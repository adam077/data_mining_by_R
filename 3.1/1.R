#install.packages("class");
library(class)

#https://en.wikipedia.org/wiki/Iris_flower_data_set
#https://zh.wikipedia.org/wiki/%E5%AE%89%E5%BE%B7%E6%A3%AE%E9%B8%A2%E5%B0%BE%E8%8A%B1%E5%8D%89%E6%95%B0%E6%8D%AE%E9%9B%86

total <- nrow(iris);
index <-sample(1:total, total*0.7)

iris.train <- iris[index, ]
iris.test <- iris[-index, ]

result.KNN <-knn(
  train=subset(iris.train, select=-Species), 
  test=subset(iris.test,select=-Species), 
  cl=iris.train$Species, k=3
)

table(iris.test$Species, result.KNN)
