library(recommenderlab)

data <- read.csv('data.csv')

rm <- as(data, "realRatingMatrix")

rec <- Recommender(
  rm, 
  method="UBCF", 
  parameter=list(
    method="euclidean",
    nn=3
  )
)

pre <- predict(rec, rm, n=1)

as(pre, 'list')
