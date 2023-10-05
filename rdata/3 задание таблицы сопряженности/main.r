library(MASS)
library(DescTools)
View(MASS)
View(DescTools)
# ������� ������ ��������� �������� � ��� ����������

table(Cars93$Type)
#������������� �������
prop.table(table(Cars93$Type))
table(Cars93$Origin)
prop.table(table(Cars93$Origin))
# ������� ������������� �� ���� ���������
tab1=table(Cars93$Type, Cars93$Origin)
tab1
# ����� �� �������� � �������
rowSums(tab1)
colSums(tab1)
# ������� ������������� �� �������� �� ���� ���������� 
prop.table(table(Cars93$Type, Cars93$Origin))
# ������� ������������� �� �������� �� ���� ���������� � ���������
prop.table(table(Cars93$Type, Cars93$Origin))*100
# ������������� ����� ������ � ������ margin=1 - ������ margin=2 - ������� ��. small in non-usa 2x more often then in usa
(tab2<-prop.table(table(Cars93$Type, Cars93$Origin), margin=2)*100)
colSums(tab2)
# test ��-������� �������� �� ���������� ����� � �������� �����������. ���� ������ 5 ->��������� �������� 
chisq.test(Cars93$Type, Cars93$Origin)

#task
light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)

seashell <- cbind(light, dark, very.dark)
seashell

chisq.test(seashell)

# ������ ���� ������ 

# seashellMatrix <- matrix(light, dark, very.dark, nco)
fisher.test(seashell) #<0.05 - �������� ��������

fisher.test(Cars93$Type, Cars93$Origin) # ������ 0.05 - �������� ��������

#GTest(Cars93$Type, Cars93$Origin)
#�������� �� ������ �� ����� (3 ��������)
table(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)
#����� ������� �����
ftable(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)
# �������� origin man trans avail ���������� �� type, ������ ��������� �������� ����� �������� ���� �������-�������-������� 
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

library(reshape) # ��� ������� melt()
drug.df <- data.frame( melt(drug,id=c("Center", "Group", "Response")))
mantelhaen.test(drug)

library(ggplot2) # ����������� �����
p <- ggplot(data = drug.df,aes(x = Center, y = value, fill = Response)) +ylab("%")
p + geom_bar(stat = "identity", position = "fill") + facet_grid(Group~.)

