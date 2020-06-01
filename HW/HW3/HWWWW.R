setwd('C:/users/mg/Desktop/Data Analytics/HW/HW3')
getwd()

mydata = read.table('HW3_CTA.csv', header=T, sep= ',')
names(mydata)
head(mydata)

library(plyr)
library(psych)
library(BSDA)

G1 = mydata$G1
G2 = mydata$G2

diff = G1 - G2

summary(diff)
describe(diff)
hist(diff)

z.test(diff,NULL,alternative="two.sided",mu=0,sigma.x=sd(diff),sigma.y=NULL,conf.level=0.99)

