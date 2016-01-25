#====================================================================================
                                          #苏瑶组作业
#====================================================================================
setwd("C:\\Users\\Administrator\\Desktop\\suyao")
mydata<-read.table("suyao5.csv",header=TRUE,sep=",")#导入数据
#------------------------------------------------------------------------------------
                              #数据处理部分
#------------------------------------------------------------------------------------
id<-c(1:1663)
mydata<-data.frame(id,mydata)                       #添加每行数据id
#------------------------------------------------------------------------------------
#summary(mydata[which(mydata$out=="1"),])
#summary(mydata[which(mydata$out=="0"),])
mydata$out<-factor(mydata$out,order=FALSE)          #将退出方式数据列设为无序因子型数据
mydata$Hytype<-factor(mydata$Hytype,order=FALSE)    #将行业类型数据列设为无序因子型数据
mydata$year<-factor(mydata$year,order=FALSE)        #将披露年份数据列设为无序因子型数据

#------------------------------------------------------------------------------------
#mydata$NrmOutDeg<-mydata$NrmOutDeg+1
#mydata$NrmOutDeg<-bcPower(mydata$NrmOutDeg,1)
#mydata$NrmInDeg<-mydata$NrmInDeg+1
#mydata$NrmInDeg<-bcPower(mydata$NrmInDeg,1)
#mydata$NrmDegree<-mydata$NrmDegree+1
#mydata$NrmDegree<-bcPower(mydata$NrmDegree,1)
#mydata$nBetweenness<-mydata$nBetweenness+1
#mydata$nBetweenness<-bcPower(mydata$nBetweenness,1)
#box.cox.powers(mydata$NrmDegree)
#box.cox.powers(mydata$nBetweenness)
#summary(z1<-powerTransform(mydata$expe))
#mydata$expe<-bcPower(mydata$expe,1)
#qqnorm(mydata$expe)
#mydata$condi<-bcPower(mydata$condi,1)
#mydata$Tztype<-bcPower(mydata$Tztype,1)
#mydata$Camp<-bcPower(mydata$Camp,1)
#mydata$NrmOutDeg<-scale(mydata$NrmOutDeg,center=T,scale=T)

#qqnorm(nBetweenness)
#mydata$NrmInDeg<-scale(mydata$NrmInDeg,center=T,scale=T)

#mydata$NrmDegree<-scale(mydata$NrmDegree,center=T,scale=T)

#mydata$nBetweenness<-scale(mydata$nBetweenness,center=T,scale=T)



mydata$expe<-mydata$expe/mean(mydata$expe)
mydata$condi<-mydata$condi/mean(mydata$condi)
mydata$Tztype<-mydata$Tztype/mean(mydata$Tztype)
mydata$Campe<-mydata$Campe/mean(mydata$Campe)
#------------------------------------------------------------------------------------
summary(mydata)                                     #对数据源mydata进行描述性统计
#------------------------------------------------------------------------------------
set.seed(1234)                                      #设置随机数种子为1234
mydata1<-mydata[sample(1663,1200),]                   #抽取建模数据集mydata1共750条数据

mydata2<-mydata[!mydata$id%in%mydata1$id,]          #构建测试集数据集mydata2为剩余166条数据
#-------------------------------------------------------------------------------------
                                #Logit模型构建
#-------------------------------------------------------------------------------------
suyao1<-glm(out~NrmInDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial())
summary(suyao1)                                     #模型一：NrmInDeg为解释变量时
#-------------------------------------------------------------------------------------
suyao2<-glm(out~NrmOutDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial())
summary(suyao2)                                     #模型二：MrmOutDeg为解释变量时
#-------------------------------------------------------------------------------------
suyao3<-glm(out~NrmDegree+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial())
summary(suyao3)                                     #模型三：NrmDegree为解释变量时
#-------------------------------------------------------------------------------------
suyao4<-glm(out~nBetweenness+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial())
summary(suyao4)                                     #模型四：nBetWeenness为解释变量时
#=====================================================================================
                               #logit模型检验（混淆矩阵）
#=====================================================================================
suyao11<-glm(out~NrmInDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial())
suyao11pre<-ifelse(predict(suyao11)>0,"1","0")
table(mydata2$out,suyao11pre)                       #模型一混淆矩阵
#-------------------------------------------------------------------------------------
suyao22<-glm(out~NrmOutDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial())
suyao22pre<-ifelse(predict(suyao22)>0,"1","0")
table(mydata2$out,suyao22pre)                       #模型二混淆矩阵
#-------------------------------------------------------------------------------------
suyao33<-glm(out~NrmDegree+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial())
suyao33pre<-ifelse(predict(suyao33)>0,"1","0")
table(mydata2$out,suyao33pre)                       #模型三混淆矩阵
#-------------------------------------------------------------------------------------
suyao44<-glm(out~nBetweenness+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial())
suyao44pre<-ifelse(predict(suyao44)>0,"1","0")
table(mydata2$out,suyao44pre)                       #模型四混淆矩阵
#=====================================================================================
                              #probit模型构建
#=====================================================================================
suyao5<-glm(out~NrmInDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial(link="probit"))
summary(suyao5)                                     #模型五：NrmInDeg为解释变量时
#-------------------------------------------------------------------------------------
suyao6<-glm(out~NrmOutDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial(link="probit"))
summary(suyao6)                                     #模型六：MrmOutDeg为解释变量时
#-------------------------------------------------------------------------------------
suyao7<-glm(out~NrmDegree+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial(link="probit"))
summary(suyao7)                                     #模型七：NrmDegree为解释变量时
#-------------------------------------------------------------------------------------
suyao8<-glm(out~nBetweenness+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata1,family=binomial(link="probit"))
summary(suyao8)                                     #模型四：nBetWeenness为解释变量时
#=====================================================================================
                              #probit模型检验（混淆矩阵）
#=====================================================================================
suyao55<-glm(out~NrmInDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial(link="probit"))
suyao55pre<-ifelse(predict(suyao55)>0,"1","0")
table(mydata2$out,suyao55pre)                       #模型五混淆矩阵
#-------------------------------------------------------------------------------------
suyao66<-glm(out~NrmOutDeg+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial(link="probit"))
suyao66pre<-ifelse(predict(suyao66)>0,"1","0")
table(mydata2$out,suyao66pre)                       #模型六混淆矩阵
#-------------------------------------------------------------------------------------
suyao77<-glm(out~NrmDegree+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial(link="probit"))
suyao77pre<-ifelse(predict(suyao77)>0,"1","0")
table(mydata2$out,suyao77pre)                       #模型七混淆矩阵
#-------------------------------------------------------------------------------------
suyao88<-glm(out~nBetweenness+expe+Tztype+times+Campe+condi+Hytype+year,data=mydata2,family=binomial(link="probit"))
suyao88pre<-ifelse(predict(suyao88)>0,"1","0")
table(mydata2$out,suyao88pre)                       #模型八混淆矩阵
#=====================================================================================