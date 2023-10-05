data=read.csv("//sysprofiles.adm.vvsu.ru/STUDENTRPROFILES$/nikitaborisenko4/Desktop/rlang/5 задание выявление зависимостей/NMES1988.csv")
#проверка нормальности распределения
#install.packages("corrplot")
library(corrplot)
#2 task
for (i in colnames(data)){
  if (i != "rownames"&& i != "nvisits"&&i != "ovisits"&&i != "novisits"&&
      i != "emergency"&&i != "hospital"&&i != "health"&&i != "adl"&&
      i != "region"&&i != "afam"&&i != "gender"&&i != "married"&&
      i != "employed"&&i != "insurance"&&i != "medicaid"){
    print(shapiro.test(data[[i]]))
    print(ks.test(data[[i]], 'rnorm'))
  }
}
#3 task матрица корреляции с отображением

dc = data.frame(data$visits, data$chronic, data$age, data$income)


corrplot(cor(dc, method='kendall'), method="square", order="AOE")

corrplot(cor(dc, method='kendall'),
         method="number", order="AOE", add=TRUE, type='lower', col='black', diag=FALSE, tl.pos ="n", cl.pos="n"
         )
# 4. проверка независимости количественных параметров
#хи-квадрат
for (i in colnames(data)){
  if (i != "rownames"&& i != "nvisits"&&i != "ovisits"&&i != "novisits"&&
      i != "emergency"&&i != "hospital"&&i != "health"&&i != "adl"&&
      i != "region"&&i != "afam"&&i != "gender"&&i != "married"&& i!= "visits"&&
      i != "employed"&&i != "insurance"&&i != "medicaid"){
    print(chisq.test(data$visits, data[[i]]))
    print(i)
  }
}
# метод корреляции (хи-кв нельзя для нормального распределения)
for (i in colnames(data)){
  if (i != "rownames"&& i != "nvisits"&&i != "ovisits"&&i != "novisits"&&
      i != "emergency"&&i != "hospital"&&i != "health"&&i != "adl"&&
      i != "region"&&i != "afam"&&i != "gender"&&i != "married"&& i!= "visits"&&
      i != "employed"&&i != "insurance"&&i != "medicaid"){
    print(cor.test(data$visits, data[[i]], method='kendall'))
    print(i)
  }
}
#< 0.05 - корреляция статически значимая
#5 task матрица сопряженности
prop.table(table(data$gender, data$health))
prop.table(table(data$gender, data$chronic))
prop.table(table(data$gender, data$employed))
#6 create factors
factor(data$health, levels=c("poor", "average", "excellent"), labels=c("0","1", '2'))
factor(data$adl , levels=c("limited", "normal"), labels=c("0","1"))
factor(data$region  , levels=c( "northeast", "midwest", "west", "other"), labels=c("0","1", "2", "3"))
factor(data$gender , levels=c("female", "male"), labels=c("0","1"))
#7 diff of val and boxplot
for (i in colnames(data)){
  if (i != "rownames"&& i != "nvisits"&&i != "ovisits"&&i != "novisits"&&
      i != "emergency"&&i != "hospital"&&i != "health"&&i != "adl"&&
      i != "region"&&i != "afam"&&i != "gender"&&i != "married"&&i != "visits"&&
      i != "employed"&&i != "insurance"&&i != "medicaid"){
    print(kruskal.test(data$visits, data[[i]]))
    boxplot(data$visits, data[[i]])
  }
}
#8 гипотезу отвергаем -> влияет

for (i in colnames(data)){
  if (i != "rownames"&& i != "nvisits"&&i != "ovisits"&&i != "novisits"&&
      i != "emergency"&&i != "hospital"&&i != "health"&&i != "adl"&&
     i != "afam"&&i != "married"&&i != "visits"&&
      i != "insurance"&&i != "medicaid"){
    print(kruskal.test(data$income , data[[i]]))
    print(i)
    boxplot(data$income , data[[i]])
  }
}
for (i in colnames(data)){
  print(kruskal.test(data$income , data[[i]]))
  print(i)
}