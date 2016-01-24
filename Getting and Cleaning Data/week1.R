#week1 1/2
mydata<-read.table("getdata-data-ss06hid.csv",header = TRUE,sep = ",")

summary(mydata$VAL==24)
mydata$FES

#week1 3
setwd("D:\\coursera\\Getting and Cleaning Data")
library(xlsx)
colindex<-7:15
rowindex<-18:23
dat<-read.xlsx("getdata-data-DATA.gov_NGAP.xlsx",sheetIndex = "NGAP Sample Data",header = TRUE,colIndex = colindex,rowIndex = rowindex)
sum(dat$Zip*dat$Ext,na.rm=T)

#---------------------------------------
#week1 4
library(XML)
a<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
c<-xmlTreeParse(sub("s", "", a),useInternal=TRUE )
c$zipcode
d<-xmlRoot(c)
names(d)
sum(xpathApply(d,"//zipcode",xmlValue)==21231)
#------------------------------------------
#week1 5
library(data.table)
DT<-fread("getdata-data-ss06pid.csv")
