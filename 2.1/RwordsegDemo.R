install.packages("rJava")
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
#install.packages("Rwordseg", repos="http://R-Forge.R-project.org", type="source")

library(Rwordseg)

d <- segmentCN("R数据挖掘实战")

segmentCN("真武七截阵和天罡北斗阵哪个厉害")

installDict('金庸武功招式.scel', '金庸武功招式')

segmentCN("真武七截阵和天罡北斗阵哪个厉害")

uninstallDict('金庸武功招式')

data <- segmentCN(
  'SogouC.mini/Sample/C000007/10.txt'
)
