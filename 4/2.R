#install.packages("psych")
library(psych)

pColumns <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width');

#主成份分析
fp <- fa.parallel(
  iris[, pColumns], 
  fa="pc", 
  n.iter=100, 
  show.legend=FALSE
)

#崖底碎石法，拿到主成份个数
pc <- principal(iris[, pColumns], nfactors=fp$ncomp)

data <- pc$scores;

#分类模型训练
kc <- kmeans(data, 3);

table(iris$Species, kc$cluster);  #查看分类概括  
prop.table(table(iris$Species, kc$cluster), 1)

#聚类结果可视化   
#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。
stripchart(
  data[,1]~kc$cluster,
  at=c(1, 1, 1),
  col=c("orange",'black', "red"), 
  pch=c(0, 1, 2)
);

points(
  kc$centers, c(1, 1, 1), 
  col = c("orange","red", 'black'), 
  pch = 8, 
  cex=2
)


#使用部分属性建模的案例，效果是没有主成份分析的好
pColumns <- c("Sepal.Length", "Sepal.Width");

data <- iris[, pColumns];

kc <- kmeans(data, 3, nstart=3);

#查看分类概括  
table(iris$Species, kc$cluster)

