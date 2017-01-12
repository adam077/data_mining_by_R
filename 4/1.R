pColumns <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width');

plot(iris[, c("Sepal.Length", "Sepal.Width")])

#分类模型训练
kmc <- kmeans(iris[, pColumns], 3);

table(iris$Species, kmc$cluster);  #查看分类概括  
prop.table(table(iris$Species, kmc$cluster), 1)

#聚类结果可视化   
#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。
plot(
  iris[c("Sepal.Length", "Sepal.Width")], 
  col = kmc$cluster, 
  pch = as.integer(iris$Species)
);

points(
  kmc$centers[, c("Sepal.Length", "Sepal.Width")], 
  col = 1:3, 
  pch = 8, 
  cex=2
)

