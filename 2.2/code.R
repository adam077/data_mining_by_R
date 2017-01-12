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

tdm <- TermDocumentMatrix(
  docs, 
  control = list(
    wordLengths= c(1, 4),
    stopwords = stopwordsCN()
  )
)

TF <- as.matrix(tdm)

IDF <- apply(TF, 1, function(row) {
  return (log2(length(docs)/(sum(ifelse(row>0, 1, 0))+1)))
});

TF.IDF = TF*IDF

keywords <- apply(TF.IDF, 2, function(col) {
  keyword <- col[order(col, decreasing=TRUE)][1:5];
  return(names(keyword))
})
#获取每篇文章关键字所在的位置，
#这样子就可以获取对应的TF、IDF以及TF.IDF的值了
keywordIndexes <- apply(TF.IDF, 2, function(col) {
  index <- order(col, decreasing=TRUE)[1:5];
  return(index)
})

TF[keywordIndexes[, 1], 1]
IDF[keywordIndexes[, 1]]
TF.IDF[keywordIndexes[, 1], 1]
