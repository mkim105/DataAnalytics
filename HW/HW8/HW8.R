getwd()
setwd('C:/users/mg/Desktop/Data Analytics/HW/HW8')
mydata=read.table('HW6_clerical_Q2.txt',header=T)

# Transform the original values in DAY to binary - weekend (Friday to Sunday) and weekday (Monday to Thursday)
library(plyr)
mydata$DAY = revalue(mydata$DAY, c("S"="Weekend"))
mydata$DAY = revalue(mydata$DAY, c("F"="Weekend"))
mydata$DAY = revalue(mydata$DAY, c("M"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("T"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("W"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("Th"="Weekday"))
head(mydata)

day = mydata$DAY
hours = mydata$HOURS
mail = mydata$MAIL
cert = mydata$CERT
acc = mydata$ACC
change = mydata$CHANGE
check = mydata$CHECK
misc = mydata$MISC
tickets = mydata$TICKETS

# let¡¯s have a practice of logistic regression by using DAY as the response variable, and all of the other variables
# (including hours) as the independent variables. Note: do not split data for this practice

# response variable : DAY
# independent variables : other variables
# Do not split data

fit <- glm(day~hours+mail+cert+acc+change+check+misc+tickets, data=mydata, family=binomial())
summary(fit) # display results
confint(fit) # 95% Confidence Interval for the coefficients
exp(coef(fit)) # compute exp(coefficients) to analyze change in odds for changes in X
exp(confint(fit)) # 95% CI for exp(coefficients), that is change in odds.

