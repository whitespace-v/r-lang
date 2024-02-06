library('readxl')
library(corrplot)
library(ggplot2)
data <-read_xlsx('./data.xlsx')
names(data)<-c('d0','d1','baranina','chicken')
dim(data)
head(data)



plot(data$baranina)
plot(data$chicken)
#������ �� ����������� ������� ����� ������� ��� ��������� ��� ����� ���������� ����� � ���������� -> ��� ��������������
#cor(bbroad$`t=time`, bbroad$butterbroad,method = "spearman")
#cor(bbroad$`t=time`, bbroad$butterbroad,method = "kendall")


#���� ���� ������� �� �������������� ���������� ���� 
library(tseries)
adf.test(data$baranina) # DF:-1.38, lag: 2, p-v: 0.8
#p.value > 0.05 -> ��� ��������������
adf.test(data$chicken) # DF:-1.36, lag: 2, p-v: 0.8
#p.value > 0.05 -> ��� �������������� 

# ������ ������������������ ������� ��� 
acf(data$baranina)
acf(data$chicken)

