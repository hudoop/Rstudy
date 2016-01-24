library(ggplot2)
library(gcookbook)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
qplot(pressure$temprature,pressure$pressure,geom = "line")
qplot(BOD$Time,BOD$demand,geom="bar")
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data = mtcars)
qplot(mpg,data = mtcars,binwidth=4)
qplot(mpg,data = mtcars)
?qplot
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
qplot(supp,len,data = ToothGrowth,geom = "boxplot")
qplot(interaction(supp,  dose),  len,  data=ToothGrowth,  geom="boxplot" )
myfun <- function(xvar) {
        1/(1 + exp(- xvar + 10))
}
qplot(c(0,20),facets=myfun, geom="line")
?qplot
p<-ggplot(data.frame(x=c(-3,3)),aes(x=x))
p+stat_function(fun = dnorm)
ggplot(pg_mean,aes(x=group,y = weight))+geom_bar(stat = "identity")
ggplot(BOD,aes(factor(Time),demand))+geom_bar(stat = "identity",fill="lightblue",colour="black")
