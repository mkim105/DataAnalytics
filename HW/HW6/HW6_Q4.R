setwd('C:/users/mg/Desktop/Data Analytics/HW/HW6')
getwd()

data4 = read.table("HW6_clerical_Q2.txt", header=T)

hours=data4$HOURS
day=data4$DAY
plot(hours~day)
table(day)
summary(data4)

# Hours of each days
mon=data4[which(day=='M'),]
tus=data4[which(day=='T'),]
wed=data4[which(day=='W'),]
thu=data4[which(day=='Th'),]
fri=data4[which(day=='F'),]
sat=data4[which(day=='S'),]

anov=lm(hours ~ day)
summary(anov)

#   c) residual analysis.
#     Plot residuals vs Predicted values:
plot( fitted(anov), rstandard(anov), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(anov))
qqline(rstandard(anov), col=2)
#     Normality Test(Shapiro-Wilk Normality Test)
shapiro.test(rstandard(anov))
