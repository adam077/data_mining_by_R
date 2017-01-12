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

#使用矩阵的方式计算
cosineDist <- function(x){
  return(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}

#字符串分隔函数
strsplits <- function(x, splits, ...) {
  for (split in splits) {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[nchar(x)>3])
}

mainSegs <- data.frame(
  name=c(NA), 
  seg1=c(NA), 
  seg2=c(NA), 
  seg3=c(NA)
);

for(i in 1:length(docs)) {
  #分句
  segs <- unlist(
    strsplits(
      docs[[i]]$content, 
      splits=c('。', '？')
    )
  );
  
  doc <- Corpus(VectorSource(c(segs, paste(docs[[i]]$content, collapse = "。"))))
  doc <- tm_map(
    doc, 
    content_transformer(segmentCN), 
    returnType="tm"
  )
  #移除空白
  doc <- tm_map(
    doc, 
    stripWhitespace
  )
  #移除标点
  doc <- tm_map(
    doc, 
    removePunctuation
  )
  doc <- tm_map(
    doc, 
    removeNumbers
  )
  tdm <- DocumentTermMatrix(
    doc, 
    control = list(
      wordLengths= c(1, 4),
      stopwords = stopwordsCN()
    )
  )
  TF <- as.matrix(tdm)
  
  cosSimilar <- cosineDist(TF)
  
  resultIndex <- apply(cosSimilar, 2, function(col) {
    return(order(col, decreasing=TRUE)[2:4]);
  })
  
  mainSegs[i, 1] <- docs[[i]]$meta$id;
  mainSegs[i, 2:4] <- segs[resultIndex[, length(segs)+1]]
}