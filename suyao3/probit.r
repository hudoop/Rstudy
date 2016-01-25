#===================================================================================
#苏瑶组作业
#===================================================================================
setwd("C:\\Users\\Administrator\\Desktop\\suyao")
mydata<-read.table("suyaoR.csv",header=TRUE,sep=",")#导入数据
#------------------------------------------------------------------------------------
#数据处理部分
#------------------------------------------------------------------------------------
id<-c(1:1663)
mydata<-data.frame(id,mydata)                       #添加每行数据id
#------------------------------------------------------------------------------------
mydata$out<-factor(mydata$out,order=FALSE)          #将退出方式数据列设为无序因子型数据
mydata$Hytype<-factor(mydata$Hytype,order=FALSE)    #将行业类型数据列设为无序因子型数据
mydata$year<-factor(mydata$year,order=FALSE)        #将披露年份数据列设为无序因子型数据

#------------------------------------------------------------------------------------
mydata$Camp<-log(mydata$Camp)                       #将市场竞争数据列取对数
#mydata$condi<-log(mydata$condi)                     #将推出条件数据列取对数

mydata$NrmOutDeg<-scale(mydata$NrmOutDeg,center=T,scale=T)
#mydata$NrmOutDeg<-ifelse((mydata$NrmOutDeg)<0,0,mydata$NrmOutDeg)
mydata$NrmInDeg<-scale(mydata$NrmInDeg,center=T,scale=T)
#mydata$NrmInDeg<-ifelse((mydata$NrmInDeg)<0,0,mydata$NrmInDeg)
mydata$NrmDegree<-scale(mydata$NrmDegree,center=T,scale=T)
#mydata$NrmDegree<-ifelse((mydata$NrmDegree)<0,0,mydata$NrmDegree)
mydata$nBetweenness<-scale(mydata$nBetweenness,center=T,scale=T)
#mydata$nBetweenness<-ifelse((mydata$nBetweenness)<0,0,mydata$nBetweenness)
mydata$condi<-scale(mydata$condi,center=T,scale=T)
mydata$condi<-ifelse((mydata$condi)<0,0,mydata$condi)
mydata$expe<-scale(mydata$expe,center=T,scale=T)
mydata$expe<-ifelse((mydata$expe)<0,0,mydata$expe)
#------------------------------------------------------------------------------------
summary(mydata)                                     #对数据源mydata进行描述性统计
#------------------------------------------------------------------------------------
set.seed(1234)                                      #设置随机数种子为1234
mydata1<-mydata[sample(1663,1200),]                 
mydata1<-rbind(mydata1,mydata[294,])                #抽取建模数据集mydata1共1201条数据

mydata2<-mydata[!mydata$id%in%mydata1$id,]          #构建测试集数据集mydata2为剩余462条数据
#-------------------------------------------------------------------------------------
#模型构建
#-------------------------------------------------------------------------------------
suyao1<-glm(out~NrmInDeg+expe+Tztype+times+condi+Hytype+year,data=mydata1,family=binomial(probit),control=list(maxit=100))
summary(suyao1)                                     #模型一：NrmInDeg为解释变量时
suyao12<-glm(out~NrmInDeg+expe+Tztype+times+condi+Hytype+year,data=mydata1,family=binomial(probit),control=list(maxit=100))
summary(suyao12)
#-------------------------------------------------------------------------------------
suyao2<-glm(out~NrmOutDeg+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata1,family=binomial(probit),control=list(maxit=100))
summary(suyao2)                                     #模型二：MrmOutDeg为解释变量时
#-------------------------------------------------------------------------------------
suyao3<-glm(out~NrmDegree+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata1,family=binomial(probit),control=list(maxit=100))
summary(suyao3)                                     #模型三：NrmDegree为解释变量时
#-------------------------------------------------------------------------------------
suyao4<-glm(out~nBetweenness+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata1,family=binomial(probit),control=list(maxit=100))
summary(suyao4)                                     #模型四：nBetWeenness为解释变量时
#=====================================================================================
#模型检验（混淆矩阵）
#=====================================================================================
suyao11<-glm(out~NrmInDeg+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata2,family=binomial(probit),control=list(maxit=100))
suyao11pre<-ifelse(predict(suyao11)>0,"1","0")
table(mydata2$out,suyao11pre)                       #模型一混淆矩阵
suyao121<-glm(out~NrmInDeg+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata2,family=binomial(probit),control=list(maxit=100))
suyao121pre<-ifelse(predict(suyao121)>0,"1","0")
table(mydata2$out,suyao121pre)
#-------------------------------------------------------------------------------------
suyao22<-glm(out~NrmOutDeg+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata2,family=binomial(probit),control=list(maxit=100))
suyao22pre<-ifelse(predict(suyao22)>0,"1","0")
table(mydata2$out,suyao22pre)                       #模型二混淆矩阵
#-------------------------------------------------------------------------------------
suyao33<-glm(out~NrmDegree+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata2,family=binomial(probit),control=list(maxit=100))
suyao33pre<-ifelse(predict(suyao33)>0,"1","0")
table(mydata2$out,suyao33pre)                       #模型三混淆矩阵
#-------------------------------------------------------------------------------------
suyao44<-glm(out~nBetweenness+expe+Tztype+times+Camp+condi+Hytype+year,data=mydata2,family=binomial(probit),control=list(maxit=100))
suyao44pre<-ifelse(predict(suyao44)>0,"1","0")
table(mydata2$out,suyao44pre)                       #模型四混淆矩阵
#=====================================================================================
