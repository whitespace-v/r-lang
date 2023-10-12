# ���������� ������� �� 30 ������, � �������� mean � 
eps1 <-rnorm(30, mean=1, sd=2.27) 
eps2 <-rnorm(30, mean=0, sd=3.07) 

#??
t=seq(from=3, to=32, by=1)

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
cor(t,eps1,method="spearman")
cor(t,eps1,method="kendall")


#����� ����� ? - �� ����������, ��� ���������; ��������� ��������

#������������������ �������
acf(xrow,lag.max=25,plot=TRUE)
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
points(bbroad$`t=time`,Tmodel$fitted.values,col='red',pch=5)
cor(Tmodel$residuals,bbroad$`t=time`,method='spearman')
#
mean(Tmodel$residuals^2)/mean(bbroad$butterbroad^2)
acf(bbroad$butterbroad,lag.max=12,plot=TRUE)
acf(Tmodel$residuals,lag.max=12,plot=TRUE)

# ���������� ������ ����� + ��������� (����������)

# ���������� ������ �����+ ��������� (����������)
library("zoo")# ����������� ���������� �������
bb=seq(from=1,to=120,by=1)# ������� ������
bb<-bbroad$butterbroad- Tmodel$fitted.values # ������ �������� ������ - ��������� � ��������
# ������� ������ �������� ��� ��������� - � � �������� � �������������
head(bb)
movbb<-rollmean(bb,5,align = c("center"))# ������ ����������� �����������
# ������ ����������� ����������� ����������� ����������
plot(bbroad$`t=time`,(Tmodel$fitted.values+movbb),col="blue")# ���������� �����
points(bbroad$`t=time`,bbroad$butterbroad,col="brown",pch=1)# �������� ��������
#������� ������ ��������� 
# ���������� ���� - ����� 


food<-read_xlsx('./foodspriceinRussia.xlsx')

head(food,4)

#����������� �������� ��������
names(food)<-c("Y","butter","milk","cheese", "sugar","tea","porreage","bread")

cor(food$Y,food$sugar,method = "spearman") #���������� ���������� ���������! ����� ���� 
cor(food$Y,food$sugar,method = "kendall")# ������������� ���������, �� ����� ������
plot(food$Y,food$sugar,col="red",pch=3)#������ �������� ������
summary(sugarT<-lm(sugar~Y,data = food))# ����������� ����� sugarT! 
points(food$Y,sugarT$fitted.values,col="black",pch=5) # ����� �� �������
