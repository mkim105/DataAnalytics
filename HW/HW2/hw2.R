setwd('C:/users/mg/Desktop/Data Analytics')
getwd()

mydata = read.table('Hww2.csv', header=T, sep= ',')
names(mydata)
head(mydata)

library(plyr)
library(psych)

wt = mydata$waiting.time
summary(wt)
describe(wt)
hist(wt)
h = hist(wt, main = "Histogram with Normal Curve")
xfit = seq(min(wt),max(wt),length=40)
yfit<-dnorm(xfit,mean=mean(wt),sd=sd(wt))
yfit<-yfit*diff(h$mids[1:2])*length(wt)
lines(xfit, yfit, col="blue", lwd=2)sgh
