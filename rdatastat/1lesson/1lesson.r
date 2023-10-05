# ���������� ������� �� 30 ������, � �������� mean � 
eps1 <-rnorm(100, mean=1, sd=2.27) 
eps2 <-rnorm(100, mean=0, sd=3.07) 

#??
t=seq(from=3, to=300, by=1)

xrow<- 1+2.05*t+eps1
yrow<- 2+1.38*t+eps2

plot(t,xrow,pch=6,col='green')
points(t,yrow,pch=2,col='brown')


cor(t,xrow,method="spearman")
cor(t,yrow,method="spearman")

#����������� ���������� �������� 
#���������� ���������� ���������
cor(t,xrow,method="kendall")
cor(t,yrow,method="kendall")

#��� ����� ����� ��������� �������������!


#��� ����� ���� ������?
cor(t,eps1,method="kendall")
cor(t,eps2,method="spearman")

#����� ����� ? - �� ����������, ��� ���������; ��������� ��������

acf(xrow,lag.max=7,plot=TRUE)
acf(yrow,lag.max=7,plot=TRUE)

#������ 
acf(eps1,lag.max=7,plot=TRUE)
acf(eps2,lag.max=7,plot=TRUE)
# ��� �������� ���������� ��� - �� ������ 30%  (0.3n (esli 100 - to lag - 30))


library('readxl')

bbroad<-read_xlsx('./butterbroad.xlsx')
head(bbroad,4)
#
dim(bbroad)

names(bbroad)<-c('month','t=time','cavier','butterbroad')
head(bbroad)
summary(bbroad)

bbroad$cavier<-as.numeric(bbroad$cavier)
cor(bbroad$`t=time`, bbroad$butterbroad,method = "spearman")
cor(bbroad$`t=time`, bbroad$butterbroad,method = "kendall")
#��������� ���������, ��������� �������������
Tmodel<-lm(bbroad$butterbroad~bbroad$`t=time`) #������ ������
summary(Tmodel) #������ ������� � ����� ������, ���� ��������������
head(Tmodel$residuals,12)#������ ������
plot(bbroad$`t=time`,bbroad$butterbroad,col='green',pch=2)
points(bbroad$`t=time`,bbroad$fitted.values,col='red',pch=5)
cor(Tmodel$residuals,bbroad$`t=time`,method='spearman')
mean(Tmodel$residuals^2)/mean(bbroad$butterbroad^2)
acf(bbroad$butterbroad,lag.max=12,plot=TRUE)
acf(Tmodel$residuals,lag.max=12,plot=TRUE)
