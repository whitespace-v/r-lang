x <- c(
  4.27296076175273, 4.3253044335640, 4.55202839528403, 
  4.33285651824668, 4.0128340310945, 4.07155452370293, 
  4.04113475664987, 2.7753693269563, 2.49186430883914, 
  2.39823431758359, 2.3789955162936, 2.14825292752989, 
  2.19583315284264, 2.1947104626036, 2.14216783486566, 
  2.28399128121205, 2.3048696257819, 2.23703873535593, 
  2.28669486582313, 2.4163169738317, 2.03782758779637, 
  2.61237703056071, 3.47620332697605)
y <- c(
  9.097731,9.512591,9.740439,9.910364,9.865059,9.935519,
  9.972640,9.920197,10.367693,10.680861,10.999012,11.246248,
  10.532816,11.144033,11.261961,11.400160,11.080695,11.173922,
  11.506877,11.105483,10.807685,6.489205,4.882802)
# ������ 
plot(x,y)
# ���� � �����
boxplot(y)
# ��� �������� �������
boxplot.stats(y)$out
# ������� ����� �������� � ����� ��������
ind <- which(y %in% boxplot.stats(y)$out)
# ��������� ����� �������� � ��������� dataframe
outler <- data.frame(x=x[ind], y=y[ind])
# �������� �� �� ����� �� �����
plot(x,y,col='blue', pch=20, ylim=c(0,max(y)))
points(outler$x, outler$y, col='red',pch=19)
# ������� ������� � ��������� �� ������ ������ ��� ���
x <- x[-ind]
y <- y[-ind]
boxplot(y)
plot(x,y,col='blue', pch=20, ylim=c(0,max(y)))
# ���� ������� - ���� �� ������� ��������, ��� ��� ���������� ����� ���������� ������������� � >7 ���������� (?)

# ������
library(outliers)
data <- c(5, 14, 15, 15, 14, 13, 19, 17, 16, 20, 22, 8, 21, 28, 11, 9, 29, 40)
grubbs.test(data)
# p.value  <0.05 -> ��������� ��������, ������ ������� ����, ���� �������� ������� - 40

grubbs.test(data, opposite= TRUE ) # �������� ������������ �������� - ������ ��� ���. �������� ���������, 5 ��� �� ������

#p<0.05, 40,42 -> ������ ������� ����� ���� �������� ��� ���������� - ���������
#� ������� ��� ������ ��������� ��������
data <- c(5, 14, 15, 15, 14, 13, 19, 17, 16, 20, 22, 8, 21, 28, 11, 9, 29, 40, 42)
grubbs.test(data, type=20)

#����� ������� - ��� �� ������������� �������� ��� ��������� ��� �������, ����������� �� +-3
# mad - ������� ���������� ����������
# ���������� ������� ���������
x=rnorm(seq(1,500),23,1)
boxplot(x)
summary(x)

#������ ������
lower_bound <- median(x) - 3 * mad(x, constant=1)
lower_bound

# ������� ������
upper_bound <- median(x) + 3 * mad(x, constant=1)
upper_bound
# ��� ��������� �� ����� ��������� ������ - ������������� �������

# ������ ����� ���������� �� �������������� ���������
outlier_ind <- which(x < lower_bound | x > upper_bound)
outlier_ind # 21 ������������� ������ (���-��)

# ������ ��� �������� 14
x[14] 

# ���� ������� ���������� �������� �� ����������� ������ - ��������, ���� �� ��������� - ������ ��� ������� �������������
# ���������� �������� ������� �� �������� ��������. 
Test1 <- dixon.test(x[1:25])
Test1 

# ���� ������� ��� ����������� ��������
test2 <- dixon.test(x[1:25],opposite = TRUE)
test2

#������������� ������ ��������� ���������� � ���������� ��� �� ��������� ��� ������. �������
boxplot(x[1:25])
boxplot(x[1:25])
out <- boxplot.stats(x[1:25])$out
boxplot(x[1:25],ylab = "x")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


#task
InsectSprays$count
plot(InsectSprays$count)
# �������� ���������� �� �����
boxplot(count ~ spray,
        xlab = "�����������",
        ylab = "���������� �������� ���������",
        main = "�������������",
        col = "violet", data = InsectSprays)

# ������ ������
lower_bound <- median(InsectSprays$count) - 3 * mad(InsectSprays$count, constant=1)
lower_bound

# ������� ������
upper_bound <- median(InsectSprays$count) + 3 * mad(InsectSprays$count, constant=1)
upper_bound

outlier_ind <- which(InsectSprays$count < lower_bound | InsectSprays$count > upper_bound)

boxplot.stats(InsectSprays$count)$out

grubbs.test(InsectSprays$count) # 0.719 - >0.05; ������� ����, 26 - ������
grubbs.test(InsectSprays$count, opposite= TRUE ) #1 -> 0 - ������
#��������� ��������
insectMedian <- median(InsectSprays$count)
#error with median
insectMedian[1]
grubbs.test(InsectSprays$count, type=20)

#����� �������
outlier_ind <- which(InsectSprays$count < lower_bound | InsectSprays$count > upper_bound)
outlier_ind # 8 69 70 71 ������������� ������� (4 �� ��� ��������� ...)

table(InsectSprays$count[c(8,69,70,71)])

#���������� �� ����������
boxplot(InsectSprays$spray)

df <- boxplot(InsectSprays$count~InsectSprays$spray)
df$out
