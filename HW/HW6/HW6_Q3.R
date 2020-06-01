setwd('C:/users/mg/Desktop/Data Analytics/HW/HW6')
getwd()

mydata = read.table("HW6_mileage.txt", header=T)

makemodel = mydata$MAKEMODEL
mpg = mydata$MPG
hp = mydata$HP
wt = mydata$WT

fit = lm(mpg~hp+wt)
summary(fit)
plot(mydata)

# a) residual analysis.
#     Plot residuals vs Predicted values:
plot( fitted(fit), rstandard(fit), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(hp, rstandard(fit), main="horse power vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

plot(wt, rstandard(fit), main="weight vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(fit))
qqline(rstandard(fit), col=2)
#     Normality Test(Shapiro-Wilk Normality Test)
shapiro.test(rstandard(fit)) #If p-value>0.05, we say it follows normal distribution at 95% confidence level

# b) Apply a transformation to the Y variable.
logmpg = log(mpg)
fit2 = lm(logmpg~hp+wt)
plot(hp, logmpg)
plot(wt, logmpg)
summary(fit2)

sqrtmpg = sqrt(mpg)
fit3 = lm(sqrtmpg~hp+wt)
plot(hp, sqrtmpg)
plot(wt, sqrtmpg)
summary(fit3)

invmpg = 1/mpg
fit4 = lm(invmpg~hp+wt)
plot(hp, invmpg)
plot(wt, invmpg)
summary(fit4)

cor(cbind(mpg,logmpg,sqrtmpg,invmpg,hp,wt))

# c) residual analysis on logmpg.

#     Plot residuals vs Predicted values:
plot( fitted(fit2), rstandard(fit2), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(hp, rstandard(fit2), main="horse power vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

plot(wt, rstandard(fit2), main="weight vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(fit2))
qqline(rstandard(fit2), col=2)
#     Normality Test(Shapiro-Wilk Normality Test)
shapiro.test(rstandard(fit2)) #If p-value>0.05, we say it follows normal distribution at 95% confidence level

# d) Interpret te estimated values of the regression parameters
summary(fit2)

# e) Use the step() function.
base = lm(logmpg~hp, data=mydata)
full = lm(logmpg~hp+wt, data=mydata)
fit5 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=T)
summary(fit5)
