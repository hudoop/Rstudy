library(DMwR)
head(algae)
summary(algae)
hist(algae$mxPH,probability = TRUE)
plot(algae$NH4,xlab = "")
abline(h=mean(algae$NH4,na.rm = T),lty=1)
abline(h=mean(algae$NH4,na.rm = T)+sd(algae$NH4,na.rm = T),lty=2)
abline(h=,median(algae$NH4,na.rm = T),lty=3)
identify(algae$NH4)
library(car)
qqplot(algae$mxPH,rnorm(length(algae$mxPH)))
library(lattice)
bwplot(size~a1,data = algae)
?bwplot
??beplot
dotplot(size~a1,data = algae)
?equal.count
z <- equal.count(rnorm(50))
plot(z)
print(z)
print(levels(z))
lines(density(algae$mxPH,na.rm = T))
hist(algae$Chla,probability = T)
lines(density(algae$Chla,na.rm = T))
algae[is.na(algae$Chla),"Chla"]<-median(algae$Chla,na.rm = T)
###########通过变量相关性填补数据缺失值
symnum(cor(algae[,4:18],use = "complete.obs"))
apply(algae,1,function(x)sum(is.na(x)))
algae[62,]
algae[199,]
data("algae")
algae<-algae[-manyNAs(algae),]
lm(PO4~oPO4,data = algae)
a<-is.na(algae$PO4)
is.na(algae$PO4)==TRUE
algae[28,"PO4"]<-42.897+1.293*algae[28,"oPO4"]
data(algae)
algae<-algae[-manyNAs(algae),]
fillPO4<-function(oP){
        if(is.na(oP))
                return(NA)
        else
                return(42.897+1.293*oP)
}

algae[is.na(algae$PO4),"PO4"]<-sapply(algae[is.na(algae$PO4),"oPO4"],fillPO4)

histogram(~mxPH|season,data = algae)
algae$season<-factor(algae$season,c("spring","summer","autumn","winter"))
lines(density(~mxPH|season,data=algae))
histogram(~mxPH|size,data = algae)
histogram(~mxPH|size*speed,data = algae)
stripplot(size~mxPH|speed,data = algae,jitter=T)
##########################
######用观测值间的想相关性填充数据













