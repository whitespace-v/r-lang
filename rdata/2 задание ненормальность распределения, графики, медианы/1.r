data=read.csv("//sysprofiles.adm.vvsu.ru/STUDENTRPROFILES$/nikitaborisenko4/Desktop/rlang/NMES1988.csv")
View(data)
str(data)


sum(is.na(data))
which(is.na(data))
data_1=na.omit(data)

shapiro.test(data$visits)
ks.test(data$visits,pnorm)
lillie.test(data$visits)
cvm.test(data$visits)
pearson.test(data$visits)


shapiro.test(data$chronic)
shapiro.test(data$age)
shapiro.test(data$school)
shapiro.test(data$income)

X <- rnorm(n=50, mean=10, sd=3) 

par(mfrow=c(2,3))

hist(X, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная X", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(X), col = "red", lwd = 2)

hist(data$visits, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная visits", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(data$visits), col = "red", lwd = 2)

hist(data$chronic, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная chronic", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(data$chronic), col = "red", lwd = 2)

hist(data$school, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная school", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(data$school), col = "red", lwd = 2)

hist(data$age, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная age", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(data$age), col = "red", lwd = 2)

hist(data$income, breaks = 20, freq = FALSE, col = "lightblue", xlab = "Переменная X", ylab = "Плотность вероятности", main = "Гистограмма, совмещенная с кривой плотности")
lines(density(data$income), col = "red", lwd = 2)

length(data$gender)

length(data$gender[data$gender=="male"])
length(data$gender[data$gender=="female"])

#6 
# стандартная ошибка среднего показателя (вероятность отклонения от среднего)
SE= sd(data$visits)/ sqrt (length(data$visits))
SE

#7 минимальное значение квартиля макстиальое значение квартиля мединаа всех данных 
summary(data$visits)
#8 медианные значения и квартили
tapply(data$visits, INDEX = data$gender, FUN= summary)
# or
table(data$gender)
table(data$gender, data$married)
#9 диограмма Ящик с усами - диаграмма размаха количества посещений по полу
boxplot(data$visits~data$gender, main="visits")
#10 талица медианных значений аргумента от 2 аргументов (ср.знач посещений 2 полов по состоянию здровья) 
tapply(data$visits, INDEX = list(data$gender,data$health), FUN= median)
#11
medians = tapply(data$visits, list(data$gender,data$insurance), median)
#создание столбиковой диограммы(данные,горизонт, кол-во цветов )
barplot(medians, beside = TRUE, col = topo.colors(2),
        legend.text = rownames(medians), xlab = "Страховка", ylab="Медиана", main= "Кол-ва посещений")



se = tapply(data$visits, list(data$gender,data$insurance), SE);se
b=barplot(medians, beside = TRUE, col = topo.colors(2),ylim=c(0,10),
          legend.text = rownames(medians), xlab = "Страховка", main= "Среднее кол-во посещений")
arrows(b, medians + se, b, medians-se, angle = 90, code = 3, length = 0.05)

#12
variable <- tapply(data$age, INDEX = list(data$married, data$gender), FUN= median)
barplot(variable, beside = TRUE, col = topo.colors(2),
        legend.text = rownames(variable), xlab = "Страховка", ylab="Медиана", main= "Медианное значение возраста")