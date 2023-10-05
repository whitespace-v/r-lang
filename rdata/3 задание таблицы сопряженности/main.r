library(MASS)
library(DescTools)
View(MASS)
View(DescTools)
# таблица частот попадания варианта в ряд наблюдений

table(Cars93$Type)
#относительные частоты
prop.table(table(Cars93$Type))
table(Cars93$Origin)
prop.table(table(Cars93$Origin))
# таблица сопряженности по двум критериям
tab1=table(Cars93$Type, Cars93$Origin)
tab1
# суммы по столбцам и строкам
rowSums(tab1)
colSums(tab1)
# таблица сопряженности по частотам по двум аргументам 
prop.table(table(Cars93$Type, Cars93$Origin))
# таблица сопряженности по частотам по двум аргументам в процентах
prop.table(table(Cars93$Type, Cars93$Origin))*100
# распределение одной группы в другой margin=1 - строки margin=2 - столбцы пр. small in non-usa 2x more often then in usa
(tab2<-prop.table(table(Cars93$Type, Cars93$Origin), margin=2)*100)
colSums(tab2)
# test хи-квадрат является ли переменные строк и столбцов независимым. если меньше 5 ->отвергаем гипотезу 
chisq.test(Cars93$Type, Cars93$Origin)

#task
light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)

seashell <- cbind(light, dark, very.dark)
seashell

chisq.test(seashell)

# точный тест фишера 

# seashellMatrix <- matrix(light, dark, very.dark, nco)
fisher.test(seashell) #<0.05 - признаки зависимы

fisher.test(Cars93$Type, Cars93$Origin) # меньше 0.05 - признаки зависимы

#GTest(Cars93$Type, Cars93$Origin)
#разбивка на группы по типам (3 аргумент)
table(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)
#более удобный вывод
ftable(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)
# критерий origin man trans avail отличается от type, учесть возможные различия можно применив тест Кокрана-Мантеля-Хензеля 
mantelhaen.test(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)
#task
drug <-array(c(11, 10, 25, 27,16, 22, 4, 10,
               14, 7, 5, 12,2, 1, 14, 16,
               6, 0, 11, 12,1, 0, 10, 10,
               1, 1, 4, 8,4, 6, 2, 1),
             dim = c(2, 2, 8),dimnames = list(
               Group = c("Drug", "Control"),
               Response = c("Success", "Failure"),
               Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug

library(reshape) # для функции melt()
drug.df <- data.frame( melt(drug,id=c("Center", "Group", "Response")))
mantelhaen.test(drug)

library(ggplot2) # графический пакет
p <- ggplot(data = drug.df,aes(x = Center, y = value, fill = Response)) +ylab("%")
p + geom_bar(stat = "identity", position = "fill") + facet_grid(Group~.)

