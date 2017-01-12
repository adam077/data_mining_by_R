library(tm)
library(tmcn)
library(Rwordseg)

docs <- Corpus(
  DirSource(
    c(
      "SogouC.mini/SampleNamed/C000007", "SogouC.mini/SampleNamed/C000008",
      "SogouC.mini/SampleNamed/C000010", "SogouC.mini/SampleNamed/C000013",
      "SogouC.mini/SampleNamed/C000014", "SogouC.mini/SampleNamed/C000016",
      "SogouC.mini/SampleNamed/C000020", "SogouC.mini/SampleNamed/C000022",
      "SogouC.mini/SampleNamed/C000023", "SogouC.mini/SampleNamed/C000024"
    )
  ),
  readerControl = list(
    language='UTF-8'
  )
)
#移除空白
docs <- tm_map(docs, stripWhitespace)
#移除标点
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, content_transformer(segmentCN), returnType="tm")

tdm <- DocumentTermMatrix(
  docs, 
  control = list(
    wordLengths= c(1, 4),
    stopwords = stopwordsCN()
  )
)

TF <- as.matrix(tdm)

#使用矩阵的方式计算
cosineDist <- function(x){
  return(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}

cosSimilar <- cosineDist(TF)

resultIndex <- apply(cosSimilar, 2, function(col) {
  return(order(col, decreasing=TRUE)[2:6]);
})

resultName <- apply(cosSimilar, 2, function(col) {
  return(names(col)[order(col, decreasing=TRUE)[2:6]]);
})
