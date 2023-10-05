data<-read.csv('./NMES1988.csv')
for (i in colnames(data)){
  # меньше чем 0.05 -> отвергаем, влияет ли фактор на величину 0: нет влияния
  print(kruskal.test(data$visits ~ data[[i]]))
  print(i)
}



#regionfactor <- factor(data$region  , levels=c( "northeast", "midwest", "west", "other"), labels=c("0","1", "2", "3"))
##wilcox.test(data$visists[regionfactor=="west"], data$visits[regionfactor=="other"], paired=FALSE)
#wilcox.test(data$visists[regionfactor=="midwest"], data$visits[regionfactor=="other"], paired=FALSE)
#wilcox.test(data$visists[regionfactor=="northeast"], data$visits[regionfactor=="other"], paired=FALSE)
# меньше чем 0.05 -> отвергаем, центры распределений смещены относительно друг друга на u (0:различия выборок несущественные)
wilcox.test(data$visists[data$region=="west"], data$visits[data$region=="other"], paired=FALSE)
wilcox.test(data$visists[data$region=="midwest"], data$visits[data$region=="other"], paired=FALSE)
wilcox.test(data$visists[data$region=="northeast"], data$visits[data$region=="other"], paired=FALSE)

wilcox.test(data$visists[data$region=="midwest"], data$visits[data$region=="west"], paired=FALSE)
wilcox.test(data$visists[data$region=="midwest"], data$visits[data$region=="northest"], paired=FALSE)
wilcox.test(data$visists[data$region=="west"], data$visits[data$region=="northest"], paired=FALSE)
boxplot(data$visists~data$region, horisontal=true)


