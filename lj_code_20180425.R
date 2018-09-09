


library(dplyr)
library(data.table)
library(magrittr)
library(purrr)
library(broom)
library(tidyr)

#导入数据
data1=fread("h:/data/lj_0901/dataset/quanji.csv",sep = ",")
#全集筛选

x1=data1%>%filter(var52>0|var53>0|var52==0&var53==0&var9==1)%>%
  filter(var8>=18&var8<=65|var5>=1&var5<=100)

#针刺伤集筛选


x2=x1%>%filter(var45==1)

#一年内针刺伤集
x3=x1%>%filter(var46==1|var46==2)


#费用集筛选

x4=x1%>%filter(var9==1)


#描述性统计分析

#省份比例
x1%>%select(var1,var2)%>%group_by(var2)%>%filter(row_number()==1)%>%ungroup()%>%
  group_by(var1)%>%summarise(医院数=n())

#省份针刺伤分布
table.sf.zcs=x1%>%select(var1,var45)%>%
  group_by(var1,var45)%>%
  summarise(针刺伤数=n())

table.sf.zcs=x1%>%
  filter(var47>=1&var47<=200)%>%
  filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%
  select(var1,var47)%>%
  group_by(var1)%>%
  summarise(zcs=sum(var47))





#医院比例
table2=as.data.frame(table(x1$var3))%>%mutate(频率=Freq/sum(Freq))

#科室比例
table3=as.data.frame(table(x1$var4))%>%mutate(频率=Freq/sum(Freq))

##护士比例
table4=as.data.frame(table(x1$var6))%>%mutate(频率=Freq/sum(Freq))

#工作年限比例
table5=as.data.frame(table(x1$var7))%>%mutate(频率=Freq/sum(Freq))

#工作中担心的职业伤害
table6_1=as.data.frame(table(x1$var10))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=1)%>%mutate(id=c(letters[1:10]))
table6_2=as.data.frame(table(x1$var11))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=2)
table6_3=as.data.frame(table(x1$var12))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=3)
table6_4=as.data.frame(table(x1$var13))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=4)
table6_5=as.data.frame(table(x1$var14))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=5)
table6_6=as.data.frame(table(x1$var15))%>%mutate(频率=Freq/sum(Freq))%>%
  filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(hurt=6)

table6=rbind(table6_1,table6_2,table6_3,table6_4,table6_5,table6_6)

#是否接受过注射相关培训
table7=as.data.frame(table(x1$var42))%>%mutate(频率=Freq/sum(Freq))

#使用胰岛素注射器，注射后回套针帽的环节中易发生针刺伤-11题
table8_1=as.data.frame(table(x1$var16))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=1)
table8_2=as.data.frame(table(x1$var17))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=2)
table8_3=as.data.frame(table(x1$var18))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=3)
table8_4=as.data.frame(table(x1$var19))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=4)
table8_5=as.data.frame(table(x1$var20))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=5)
table8_6=as.data.frame(table(x1$var21))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=6)
table8_7=as.data.frame(table(x1$var22))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=7)
table8_8=as.data.frame(table(x1$var23))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=8)
table8_9=as.data.frame(table(x1$var24))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第11题=9)

table8=rbind(table8_1,table8_2,table8_3,table8_4,table8_5,table8_6,table8_7,table8_8,table8_9)

#12题-使用胰岛素笔的过程
table9_1=as.data.frame(table(x1$var25))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=1)
table9_2=as.data.frame(table(x1$var26))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=2)
table9_3=as.data.frame(table(x1$var27))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=3)
table9_4=as.data.frame(table(x1$var28))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=4)
table9_5=as.data.frame(table(x1$var29))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=5)
table9_6=as.data.frame(table(x1$var30))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=6)
table9_7=as.data.frame(table(x1$var31))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=7)
table9_8=as.data.frame(table(x1$var32))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=8)
table9_9=as.data.frame(table(x1$var33))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=9)
table9_10=as.data.frame(table(x1$var34))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第12题=10)

table9=rbind(table9_1,table9_2,table9_3,table9_4,table9_5,table9_6,table9_7,table9_8,table9_9,table9_10)

#13题——卸除使用过的胰岛素笔用针头
table10_1=as.data.frame(table(x1$var35))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=1)
table10_2=as.data.frame(table(x1$var36))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=2)
table10_3=as.data.frame(table(x1$var37))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=3)
table10_4=as.data.frame(table(x1$var38))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=4)
table10_5=as.data.frame(table(x1$var39))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=5)
table10_6=as.data.frame(table(x1$var40))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=6)
table10_7=as.data.frame(table(x1$var41))%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)%>%mutate(第13题=7)

table10=rbind(table10_1,table10_2,table10_3,table10_4,table10_5,table10_6,table10_7)

#胰岛素注射器和胰岛素笔使用比例
#胰岛素注射器和胰岛素笔使用次数
table11_1=as.data.frame(mean(x1$var43))
table11_2=as.data.frame(mean(x1$var44))

table11_3=as.data.frame(mean(x1$var52))
table11_4=as.data.frame(mean(x1$var53))

table11=rbind(table11_1,table11_2,table11_3,table11_4)



#一年内针刺伤的集
x3=x1%>%filter(var46==1|var46==2)


zcs=x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)



#每千人一年内发生针刺伤的次数
table12=x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%select(var47)%>%sum%>%'/'(9873)*1000


#注射器次数
x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%select(var48)%>%sum%>%'/'(9873)*1000
#注射笔次数
x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%select(var49)%>%sum%>%'/'(9873)*1000

#注射器刺伤年发生率

(zcs%>%select(var48)%>%sum)/(x1%>%filter(var52<=200)%>%filter(var53<=200)%>%select(var52)%>%'*'(52)%>%sum)

(zcs%>%select(var48)%>%sum)


#注射笔刺伤年发生率
x1%>%filter(var52<=200)%>%filter(var53<=200)%>%select(var53)%>%mutate(分母=sum(var53*52))%>%select(分母)%>%mutate(分子=1377)%>%mutate(年发生率=分子/分母)

zcs%>%select(var49)%>%sum


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。
#前端
(zcs%>%select(var50)%>%sum)/(x1%>%filter(var52<=200)%>%filter(var53<=200)%>%select(var53)%>%sum)

#后端
(zcs%>%select(var51)%>%sum)/(x1%>%filter(var52<=200)%>%filter(var53<=200)%>%select(var53)%>%sum)

#每千人一年内前端
x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%select(var50)%>%sum%>%'/'(9873)*1000

#每千人一年内后端
x3%>%filter(var47>=1&var47<=200)%>%filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%select(var51)%>%sum%>%'/'(9873)*1000




#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。20170826增加的处理结果

#31页PPT。。。。。。。。。。。。。。。、
#table12之后的结果
#按照每周注射次数分组（小于8，大于8小于50，大于50）针刺伤发生次数，每千人针刺伤发生次数
ppt31=x1%>%mutate(var5253=var52+var53)%>%filter(var52>=0&var52<=200)%>%filter(var53>=0&var53<=200)

ppt31$var5253[ppt31$var5253<8]<-"小于8"
ppt31$var5253[ppt31$var5253>=8&ppt31$var5253<=50]<-"8到50"
ppt31$var5253[ppt31$var5253>50]<-"大于50"

table13=ppt31%>%filter(var47>=1&var47<=200)%>%
  filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%
  group_by(var5253)%>%
  summarise(ppt31针刺伤次数=sum(var47),例数=n())%>%mutate(每千人针刺伤发生次数人次=ppt31针刺伤次数/例数*1000)


#35页PPT——按照科室分组计算
#全集科室占比
table14_1=as.data.frame(table(x1$var4))%>%mutate(频率=Freq/sum(Freq))%>%rename(var4=Var1,科室占比=频率)


#每千人一年内针刺伤发生次数
#各科室一年内针刺伤发生次数/全集人数=每千人一年内发生针刺伤次数（人次）
table14_3=x3%>%filter(var47>=1&var47<=200)%>%
  filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%
  group_by(var4)%>%summarise(针刺伤发生次数=sum(var47))

table14_4=x2%>%
  group_by(var4)%>%summarise(科室针刺伤人数=n())



table14_5=x1%>%group_by(var4)%>%summarise(全集人数=n())


table14_6=list(table14_5,table14_4,table14_3)%>%reduce(full_join,by="var4")%>%
  mutate(针刺伤人群占比=科室针刺伤人数/全集人数,每千人一年内针刺伤发生次数=针刺伤发生次数/全集人数*1000)

#科室占比+其他针刺伤人群占比+每千人一年内针刺伤发生次数


table14_6$var4=as.factor(table14_6$var4)

table14=full_join(table14_1,table14_6,by="var4")
#....................................................................

#按照省份分组计算

#全集省份占比
table15_1=as.data.frame(table(x1$var1))%>%mutate(频率=Freq/sum(Freq))%>%rename(var1=Var1,省市占比=频率)


#每千人一年内针刺伤发生次数
#各科室一年内针刺伤发生次数/全集人数=每千人一年内发生针刺伤次数（人次）
table15_3=x3%>%filter(var47>=1&var47<=200)%>%
  filter(var47==var48+var49&var48>=0&var49==var50+var51&var51>=0)%>%
  group_by(var1)%>%summarise(针刺伤发生次数=sum(var47))

table15_4=x2%>%
  group_by(var1)%>%summarise(省市针刺伤人数=n())



table15_5=x1%>%group_by(var1)%>%summarise(全集人数=n())


table15_6=list(table15_5,table15_4,table15_3)%>%reduce(full_join,by="var1")%>%
  mutate(针刺伤人群占比=省市针刺伤人数/全集人数,每千人一年内针刺伤发生次数=针刺伤发生次数/全集人数*1000)

#科室占比+其他针刺伤人群占比+每千人一年内针刺伤发生次数


table15_6$var1=as.factor(table15_6$var1)

table15=full_join(table15_1,table15_6,by="var1")


#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。


#20170827

#是否上报科室的统计-针刺伤集x2
table16=as.data.frame(table(x2$var54))%>%mutate(上报情况=Freq/sum(Freq))%>%rename(是否上报=Var1)


#23题，var55-var60，上报原因的统计----针刺伤集x2
table8_1=as.data.frame(table(x2$var55))%>%mutate(上报原因=1)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)
table8_2=as.data.frame(table(x2$var56))%>%mutate(上报原因=2)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)
table8_3=as.data.frame(table(x2$var57))%>%mutate(上报原因=3)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)
table8_4=as.data.frame(table(x2$var58))%>%mutate(上报原因=4)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)
table8_5=as.data.frame(table(x2$var59))%>%mutate(上报原因=5)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)
table8_6=as.data.frame(table(x2$var60))%>%mutate(上报原因=6)%>%mutate(频率=Freq/sum(Freq))%>%filter(Var1!=0)%>%select(-Freq,-Var1)

table17=rbind(table8_1,table8_2,table8_3,table8_4,table8_5,table8_6)

#var61统计，是否按照医院标准流程处理------针刺伤集x2
table18=as.data.frame(table(x2$var61))%>%mutate(处理方式频率=Freq/sum(Freq))%>%rename(是否按照标准流程=Var1)

#var63统计，是否有离开工作的想法-------------针刺伤集x2
table19=as.data.frame(table(x2$var63))%>%mutate(频率=Freq/sum(Freq))%>%rename(是否有离开工作的想法=Var1)


#胰岛素注射针刺伤影响因素（单因素）

#分析医院级别与针刺伤是否相互独立。。。。。。。。。。。。。。。。。。。。。。。卡方检验
cq1_1=xtabs(~var3+var45,x1)%>%chisq.test()%>%tidy()
cq1_2=xtabs(~var3+var45,x1)%>%fisher.test()%>%tidy()


cq1=cbind(cq1_1,cq1_2)
#分析科室与针刺伤是否相互独立。。。。。。。。。。。。。。。。。。。。。。。卡方检验
cq2=xtabs(~var4+var45,x1)%>%chisq.test()%>%tidy()

#科室频数表

table_ks=xtabs(~var4+var45,x1)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="table_ks.csv",sep=",",row.names=F)

#分析职称与针刺伤是否独立。。。。。。。。。。。。。。。。。。。。。。。卡方检验
cq3=xtabs(~var6+var45,x1)%>%chisq.test()%>%tidy()

xtabs(~var6+var45,x1)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="table_ks1.csv",sep=",",row.names=F)

#分析工作年限与针刺伤是否独立。。。。。。。。。。。。。。。。。。。。。。。卡方检验
cq4=xtabs(~var7+var45,x1)%>%chisq.test()%>%tidy()

xtabs(~var7+var45,x1)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="table_ks2.csv",sep=",",row.names=F)


#分析床位数与针刺伤是否独立。。。。。。。。。。。。。。。。。。。。。。。t检验
t_data=x1%>%filter(var5>=1&var5<=100)
cq5=t.test(var5~var45,data = t_data)%>%tidy()
cq5_sd=t_data%>%group_by(var45)%>%summarise(标准差=sd(var5),均值=mean(var5))


#logistic单因素分析
#胰岛素注射笔的结果。。。。。。。。。。。。。。。。。。。。。。。单因素logistic回归
fit1_coef=glm(var45~var43,data = x1)%>%step%>%coef%>%exp%>%as.data.frame()
fit1_confint=glm(var45~var43,data = x1)%>%step%>%confint%>%exp

cq6=cbind(fit1_coef,fit1_confint)%>%
  write.table(file="cq6.csv",sep=",",row.names=F)

#胰岛素注射器的结果。。。。。。。。。。。。。。。。。。。。。。。单因素logistic回归
fit2_coef=glm(var45~var44,data = x1)%>%step%>%coef%>%exp%>%as.data.frame()
fit2_confint=glm(var45~var44,data = x1)%>%step%>%confint%>%exp

cq7=cbind(fit2_coef,fit2_confint)%>%
  write.table(file="cq7.csv",sep=",",row.names=F)

#每周注射总次数。。。。。。。。。。。。。。。。。。。。。。。。。。t检验
cq8=xtabs(~var5253+var45,x1)%>%t.test()%>%tidy()
cq8_sd=x1%>%group_by(var45)%>%summarise(标准差=sd(var5253))


#是否接受过安全培训。。。。。。。。。。。。。。。。。。。。。。。。卡方检验
cq9=xtabs(~var42+var45,x1)%>%chisq.test()%>%tidy()



#................................................................20170828......反向问题变量赋值
#将1变为5,3变为6，4变为1,2变为3,5变为4,6变为2
h1=x1    #修改反向问题之后的数据集

#var145
h1$var145[h1$var145==1]<-5
h1$var145[h1$var145==3]<-6
h1$var145[h1$var145==4]<-1
h1$var145[h1$var145==2]<-3
h1$var145[h1$var145==5]<-4
h1$var145[h1$var145==6]<-2

#var149
h1$var149[h1$var149==1]<-5
h1$var149[h1$var149==3]<-6
h1$var149[h1$var149==4]<-1
h1$var149[h1$var149==2]<-3
h1$var149[h1$var149==5]<-4
h1$var149[h1$var149==6]<-2

#var153
h1$var153[h1$var153==1]<-5
h1$var153[h1$var153==3]<-6
h1$var153[h1$var153==4]<-1
h1$var153[h1$var153==2]<-3
h1$var153[h1$var153==5]<-4
h1$var153[h1$var153==6]<-2

#var157
h1$var157[h1$var157==1]<-5
h1$var157[h1$var157==3]<-6
h1$var157[h1$var157==4]<-1
h1$var157[h1$var157==2]<-3
h1$var157[h1$var157==5]<-4
h1$var157[h1$var157==6]<-2

#var159
h1$var159[h1$var159==1]<-5
h1$var159[h1$var159==3]<-6
h1$var159[h1$var159==4]<-1
h1$var159[h1$var159==2]<-3
h1$var159[h1$var159==5]<-4
h1$var159[h1$var159==6]<-2

#var162
h1$var162[h1$var162==1]<-5
h1$var162[h1$var162==3]<-6
h1$var162[h1$var162==4]<-1
h1$var162[h1$var162==2]<-3
h1$var162[h1$var162==5]<-4
h1$var162[h1$var162==6]<-2

#var165
h1$var165[h1$var165==1]<-5
h1$var165[h1$var165==3]<-6
h1$var165[h1$var165==4]<-1
h1$var165[h1$var165==2]<-3
h1$var165[h1$var165==5]<-4
h1$var165[h1$var165==6]<-2

#var166
h1$var166[h1$var166==1]<-5
h1$var166[h1$var166==3]<-6
h1$var166[h1$var166==4]<-1
h1$var166[h1$var166==2]<-3
h1$var166[h1$var166==5]<-4
h1$var166[h1$var166==6]<-2

#var171
h1$var171[h1$var171==1]<-5
h1$var171[h1$var171==3]<-6
h1$var171[h1$var171==4]<-1
h1$var171[h1$var171==2]<-3
h1$var171[h1$var171==5]<-4
h1$var171[h1$var171==6]<-2

#var172
h1$var172[h1$var172==1]<-5
h1$var172[h1$var172==3]<-6
h1$var172[h1$var172==4]<-1
h1$var172[h1$var172==2]<-3
h1$var172[h1$var172==5]<-4
h1$var172[h1$var172==6]<-2

#var174
h1$var174[h1$var174==1]<-5
h1$var174[h1$var174==3]<-6
h1$var174[h1$var174==4]<-1
h1$var174[h1$var174==2]<-3
h1$var174[h1$var174==5]<-4
h1$var174[h1$var174==6]<-2

#var176
h1$var176[h1$var176==1]<-5
h1$var176[h1$var176==3]<-6
h1$var176[h1$var176==4]<-1
h1$var176[h1$var176==2]<-3
h1$var176[h1$var176==5]<-4
h1$var176[h1$var176==6]<-2

#var177
h1$var177[h1$var177==1]<-5
h1$var177[h1$var177==3]<-6
h1$var177[h1$var177==4]<-1
h1$var177[h1$var177==2]<-3
h1$var177[h1$var177==5]<-4
h1$var177[h1$var177==6]<-2

#var178
h1$var178[h1$var178==1]<-5
h1$var178[h1$var178==3]<-6
h1$var178[h1$var178==4]<-1
h1$var178[h1$var178==2]<-3
h1$var178[h1$var178==5]<-4
h1$var178[h1$var178==6]<-2

#var180
h1$var180[h1$var180==1]<-5
h1$var180[h1$var180==3]<-6
h1$var180[h1$var180==4]<-1
h1$var180[h1$var180==2]<-3
h1$var180[h1$var180==5]<-4
h1$var180[h1$var180==6]<-2


#???	添加变量anxiety_score=var141+….+var160 变成anxiety_score=int(1.25X) 
h1_anxi=h1%>%mutate(anxiety_score=var141+var142+var143+var144+var145+var146+var147+var148+var149+var150+
                      var151+var152+var153+var154+var155+var156+var157+var158+var159+var160)

h1_anxi$anxiety_score=h1_anxi$anxiety_score%>%'*'(1.25)%>%round() 

#???	添加变量depression_score=var161+…+var180变成depression_score=int(1.25X)
h1_depr=h1_anxi%>%mutate(depression_score=var161+var162+var163+var164+var165+var166+var167+var168+var169+
                           var170+var171+var172+var173+var174+var175+var176+var177+var178+var179+var180)

h1_depr$depression_score=h1_depr$depression_score%>%'*'(1.25)%>%round()

#通过标准分划分无焦虑、轻度焦虑、中度焦虑;;;无抑郁、轻度抑郁、中度抑郁；；；mental=1;0
#划分焦虑的标准
h1_1=h1_depr%>%filter(anxiety_score<50)%>%mutate(anxiety="没有焦虑")
h1_2=h1_depr%>%filter(anxiety_score>=50&anxiety_score<60)%>%mutate(anxiety="轻度焦虑")  
h1_3=h1_depr%>%filter(anxiety_score>=60&anxiety_score<70)%>%mutate(anxiety="中度焦虑")  
h1_4=h1_depr%>%filter(anxiety_score>=70)%>%mutate(anxiety="重度焦虑")  

h1_depr=rbind(h1_1,h1_2,h1_3,h1_4)

#划分抑郁的标准
h1_1=h1_depr%>%filter(depression_score<50)%>%mutate(depression="没有抑郁")
h1_2=h1_depr%>%filter(depression_score>=50&depression_score<60)%>%mutate(depression="轻度抑郁")  
h1_3=h1_depr%>%filter(depression_score>=60&depression_score<70)%>%mutate(depression="中度抑郁")  
h1_4=h1_depr%>%filter(depression_score>=70)%>%mutate(depression="重度抑郁")  

h1_depr=rbind(h1_1,h1_2,h1_3,h1_4)

#划分精神状态的好坏
h1_1=h1_depr%>%filter(depression_score<50&anxiety_score<50)%>%mutate(mental=0)
h1_2=h1_depr%>%filter(depression_score>=50|anxiety_score>=50)%>%mutate(mental=1)

h1_depr=rbind(h1_1,h1_2)


#.....................................................................................卡方检验

#。。。。。。。。。。。。。。。。。..................................................焦虑程度与针刺伤是否独立

#焦虑程度
cq10=xtabs(~anxiety+var45,h1_depr)%>%chisq.test()%>%tidy()
xtabs(~anxiety+var45,h1_depr)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="cq10.csv",sep=",",row.names=F)

#抑郁程度
cq11=xtabs(~depression+var45,h1_depr)%>%chisq.test()%>%tidy()
xtabs(~depression+var45,h1_depr)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="cq11.csv",sep=",",row.names=F)

#....................................................................................一年内针刺伤集

h1_depr_1=h1_depr%>%filter(var46==1|var46==2)

#焦虑程度与3月内-3月至一年内是否独立

cq14=xtabs(~mental+var46,h1_depr_1)%>%chisq.test()%>%tidy()
xtabs(~anxiety+var46,h1_depr_1)%>%as.data.frame()%>%spread(key="var46",value="Freq")%>%
  write.table(file="cq14.csv",sep=",",row.names=F)

#抑郁程度与3月内-3月至一年内是否独立

cq15=xtabs(~depression+var46,h1_depr_1)%>%chisq.test()%>%tidy()
xtabs(~depression+var46,h1_depr_1)%>%as.data.frame()%>%spread(key="var46",value="Freq")%>%
  write.table(file="cq15.csv",sep=",",row.names=F)


#..............................................................................................................
#是否徒手,徒手=1，非徒手=0
h1_1=h1_depr%>%filter(var35==0&var36==0)%>%mutate(tushou="0")
h1_2=h1_depr%>%filter(var35!=0|var36!=0)%>%mutate(tushou="1")

h1_depr=rbind(h1_1,h1_2)
#是否使用工具，使用工具=1，非使用工具=0
h1_1=h1_depr%>%filter(var39==0&var40==0&var41==0)%>%mutate(gongju="0")
h1_2=h1_depr%>%filter(var39!=0|var40!=0|var41!=0)%>%mutate(gongju="1")

h1_depr=rbind(h1_1,h1_2)

#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。卡方检验
#.徒手非徒手
cq12=xtabs(~tushou+var45,h1_depr)%>%chisq.test()%>%tidy()
xtabs(~tushou+var45,h1_depr)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="cq12.csv",sep=",",row.names=F)


#.使用工具非使用工具
cq13=xtabs(~gongju+var45,h1_depr)%>%chisq.test()%>%tidy()
xtabs(~gongju+var45,h1_depr)%>%as.data.frame()%>%spread(key="var45",value="Freq")%>%
  write.table(file="cq13.csv",sep=",",row.names=F)



#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。20170828处理过程
#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。


#
#胰岛素注射笔的结果。。。。。。。。。。。。。。。。。。。。。。。多因素logistic回归


#.....................................................20170829

#是否发生针刺伤的logistic回归————————去掉mental

h1_depr$var45[h1_depr$var45==2]<-0
h1_depr$var3[h1_depr$var3==3]<-4
h1_depr$var3[h1_depr$var3==1]<-3
h1_depr$var3[h1_depr$var3==4]<-1

h1_depr=h1_depr%>%mutate(var5253=var52+var53)%>%filter(var52>=0&var52<=200)%>%filter(var53>=0&var53<=200)

h1_depr$var5253=as.numeric(h1_depr$var5253)
h1_depr$var5253[h1_depr$var5253<8]<-1
h1_depr$var5253[h1_depr$var5253>=8&h1_depr$var5253<=50]<-2
h1_depr$var5253[h1_depr$var5253>50]<-3
h1_depr$var5253=as.factor(as.character(h1_depr$var5253))
#p值

fit_step_var45=glm(factor(var45)~factor(var3)+factor(var4)+factor(var6)+factor(var7)+var8+factor(gongju)+factor(tushou)+factor(var42)+
                     var43+var44+var5253+var3:var5253+var4:var5253+var6:var5253,
                   data = h1_depr,family = binomial(link = "logit"))%>%step()

fit_step_p=fit_step_var45%>%summary()



#逐步回归指数比
fit_step_coef_exp=fit_step_var45%>%coef()%>%exp()%>%tidy()


#逐步回归置信区间
fit_step_confint_exp=fit_step_var45%>%confint()%>%exp()%>%tidy()

cbind(fit_step_p$coefficients[,4],fit_step_coef_exp,fit_step_confint_exp)%>%write.table(file="0425针刺伤逐步回归指数比加置信区间.csv",sep=",",row.names=F)


###是否发生针刺伤的logistic回归————————保留mental


#p值

fit_step_var45=glm(factor(var45)~factor(var3)+factor(var4)+factor(var6)+factor(var7)+var8+factor(gongju)+factor(tushou)+factor(var42)+
                     var43+var44+var5253+var3:var5253+var4:var5253+var6:var5253+factor(mental),
                   data = h1_depr,family = binomial(link = "logit"))%>%step()

fit_step_p=fit_step_var45%>%summary()



#逐步回归指数比
fit_step_coef_exp=fit_step_var45%>%coef()%>%exp()%>%tidy()


#逐步回归置信区间
fit_step_confint_exp=fit_step_var45%>%confint()%>%exp()%>%tidy()

cbind(fit_step_p$coefficients[,4],fit_step_coef_exp,fit_step_confint_exp)%>%write.table(file="0425针刺伤保留mental逐步回归指数比加置信区间.csv",sep=",",row.names=F)



#针刺伤集x2
#var64的logistic回归
h1_depr_1=h1_depr%>%filter(var45==1)%>%filter(var52<=200)%>%filter(var53<=200)


#逐步回归指数比
var64_fit_step_coef_exp=glm(factor(var64)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                              factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                            data = h1_depr_1,family = binomial(link = "logit"))%>%step()%>%coef()%>%exp()%>%tidy()

#逐步回归置信区间
var64_fit_step_confint_exp=glm(factor(var64)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                                 factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                               data = h1_depr_1,family = binomial(link = "logit"))%>%step()%>%confint()%>%exp()%>%tidy()

cbind(fit_step_coef_exp,fit_step_confint_exp)%>%write.table(file="var64_逐步回归指数比加置信区间.csv",sep=",",row.names=F)

#var65
#逐步回归指数比
var65_fit_step_coef_exp=glm(factor(var65)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                              factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                            data =h1_depr_1 ,family = binomial(link = "logit"))%>%step()%>%coef()%>%exp()%>%tidy()

#逐步回归置信区间
var65_fit_step_confint_exp=glm(factor(var65)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                                 factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                               data = h1_depr_1,family = binomial(link = "logit"))%>%step()%>%confint()%>%exp()%>%tidy()

cbind(fit_step_coef_exp,fit_step_confint_exp)%>%write.table(file="var65_逐步回归指数比加置信区间.csv",sep=",",row.names=F)

#var67
var67_fit_step_coef_exp=glm(factor(var67)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                              factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                            data =h1_depr_1 ,family = binomial(link = "logit"))%>%step()%>%coef()%>%exp()%>%tidy()


var67_fit_step_confint_exp=glm(factor(var67)~factor(var3)+factor(var4)+factor(var5)+factor(var6)+factor(var7)+factor(var42)+
                                 factor(var61)+factor(var47)+factor(tushou)+factor(gongju)+factor(mental),
                               data = h1_depr_1,family = binomial(link = "logit"))%>%step()%>%confint()%>%exp()%>%tidy()

cbind(fit_step_coef_exp,fit_step_confint_exp)%>%write.table(file="var67_逐步回归指数比加置信区间.csv",sep=",",row.names=F)

#............................................................................................全集-不逐步回归
























