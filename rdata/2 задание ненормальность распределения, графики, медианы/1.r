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

hist(X, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� X", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(X), col = "red", lwd = 2)

hist(data$visits, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� visits", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(data$visits), col = "red", lwd = 2)

hist(data$chronic, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� chronic", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(data$chronic), col = "red", lwd = 2)

hist(data$school, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� school", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(data$school), col = "red", lwd = 2)

hist(data$age, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� age", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(data$age), col = "red", lwd = 2)

hist(data$income, breaks = 20, freq = FALSE, col = "lightblue", xlab = "���������� X", ylab = "��������� �����������", main = "�����������, ����������� � ������ ���������")
lines(density(data$income), col = "red", lwd = 2)

length(data$gender)

length(data$gender[data$gender=="male"])
length(data$gender[data$gender=="female"])

#6 
# ����������� ������ �������� ���������� (����������� ���������� �� ��������)
SE= sd(data$visits)/ sqrt (length(data$visits))
SE

#7 ����������� �������� �������� ����������� �������� �������� ������� ���� ������ 
summary(data$visits)
#8 ��������� �������� � ��������
tapply(data$visits, INDEX = data$gender, FUN= summary)
# or
table(data$gender)
table(data$gender, data$married)
#9 ��������� ���� � ����� - ��������� ������� ���������� ��������� �� ����
boxplot(data$visits~data$gender, main="visits")
#10 ������ ��������� �������� ��������� �� 2 ���������� (��.���� ��������� 2 ����� �� ��������� �������) 
tapply(data$visits, INDEX = list(data$gender,data$health), FUN= median)
#11
medians = tapply(data$visits, list(data$gender,data$insurance), median)
#�������� ����������� ���������(������,��������, ���-�� ������ )
barplot(medians, beside = TRUE, col = topo.colors(2),
        legend.text = rownames(medians), xlab = "���������", ylab="�������", main= "���-�� ���������")



se = tapply(data$visits, list(data$gender,data$insurance), SE);se
b=barplot(medians, beside = TRUE, col = topo.colors(2),ylim=c(0,10),
          legend.text = rownames(medians), xlab = "���������", main= "������� ���-�� ���������")
arrows(b, medians + se, b, medians-se, angle = 90, code = 3, length = 0.05)

#12
variable <- tapply(data$age, INDEX = list(data$married, data$gender), FUN= median)
barplot(variable, beside = TRUE, col = topo.colors(2),
        legend.text = rownames(variable), xlab = "���������", ylab="�������", main= "��������� �������� ��������")