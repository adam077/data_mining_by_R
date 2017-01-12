#install.packages("tm");
#install.packages("tmcn", repos="http://R-Forge.R-project.org");

library(tm)
library(tmcn)

#按目录读入语料库
C000007 <- Corpus(
  DirSource(
    'SogouC.mini/Sample/C000007'
  ), 
  readerControl = list(language="UTF-8")
)

stopwordsCN()

library(Rwordseg)

C000007 <- tm_map(C000007, stripWhitespace)
C000007 = tm_map(C000007, content_transformer(segmentCN), returnType='tm')

control = list(
  wordLengths = c(1, 4),
  stopwords = stopwordsCN()
)

#转成向量矩阵
mt <- TermDocumentMatrix(C000007, control=control)
mt$dimnames
dmt <- as.matrix(mt)
fix(dmt)

#install.packages("wordcloud")
library(wordcloud)
v <- sort(rowSums(dmt), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(
  d$word, 
  d$freq, 
  min.freq=3, 
  random.order=F, 
  colors=rainbow(length(row.names(dmt)))
)