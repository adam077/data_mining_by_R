pColumns <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width');

data <- iris[ , pColumns]

data.e <- dist(data)

data.m <- as.matrix(data.e)

model <- hclust(data.e)

plot(model)

result <- cutree(model, k=3)

table(iris[, 5], result)
