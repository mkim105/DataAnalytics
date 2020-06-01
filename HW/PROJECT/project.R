setwd('C:/users/mg/Desktop/Data Analytics/HW/PROJECT/')
getwd()

mydata = read.table("winequality-red.csv", header=T, sep=',')

fa = mydata$fixed.acidity
va = mydata$volatile.acidity
ca = mydata$citric.acid
rs = mydata$residual.sugar
cs = mydata$chlorides
fs = mydata$free.sulfur.dioxide
ts = mydata$total.sulfur.dioxide
ds = mydata$density
ph = mydata$pH
ss = mydata$sulphates
al = mydata$alcohol
qy = mydata$quality
qy2 = qy*qy
qy3 = 1/qy
qy4 = sqrt(qy)

cor(cbind(qy2,fa,va,ca,rs,cs,fs,ts,ds,ph,ss,al))
  
plot(mydata)
cor(mydata)

cor(cbind(cc4,cmt,bf,fa,wt,sp,cA,fA,age))

fit = lm(cc~cmt+bf+fa+wt+sp+cA+fA+age)
summary(fit)
plot(mydata)

fit2 = lm(cc~cmt+bf+fa+wt+sp+age)
summary(fit2)
