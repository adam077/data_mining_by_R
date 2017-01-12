library(tm)
library(tmcn)
library(Rwordseg)

docs <- Corpus(
  DirSource(
    c(
      "SogouC.mini/Sample/C000007", "SogouC.mini/Sample/C000008",
      "SogouC.mini/Sample/C000010", "SogouC.mini/Sample/C000013",
      "SogouC.mini/Sample/C000014", "SogouC.mini/Sample/C000016",
      "SogouC.mini/Sample/C000020", "SogouC.mini/Sample/C000022",
      "SogouC.mini/Sample/C000023", "SogouC.mini/Sample/C000024"
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

cosineDist <- function(textMatrix) {
  cosSimilar <- matrix(nrow=nrow(textMatrix), ncol=nrow(textMatrix))
  
  for(i in 1:nrow(textMatrix)) {
    #对角线数据
    cosSimilar[i, i] <- 1;
    #处理i到了nrow(textMatrix)的异常
    if(i==nrow(textMatrix)) {
      break;
    }
    for(j in (i+1):nrow(textMatrix)) {
      cosSimilar[i, j] <- 
        sum(textMatrix[i, ] * textMatrix[j, ]) / 
        (sqrt(sum(textMatrix[i, ]^2)) * sqrt(sum(textMatrix[j, ]^2)));
      cosSimilar[j, i] <- cosSimilar[i, j];
    }
  }
  return(cosSimilar)
}

cosSimilar <- cosineDist(TF)

resultIndex <- apply(cosSimilar, 2, function(col) {
  return(order(col, decreasing=TRUE)[2:6]);
})

resultName <- apply(cosSimilar, 2, function(col) {
  return(names(col)[order(col, decreasing=TRUE)[2:6]]);
})
