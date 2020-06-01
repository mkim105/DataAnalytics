#Logistic Regression
getwd()
setwd('C:/users/mg/Desktop/Data Analytics/HW/HW8')
mydata=read.table('HW6_clerical_Q2.txt',header=T)
#install.packages("dummies")
#install.packages("caret")
#install.packages("leaps")
#install.packages("nnet")
#install.packages("boot")
library(car)# for use N-fold cross validataion
library(nnet)
library(dummies)
library(plyr) #for using revalue weekdays

# Transform the original values in DAY to binary - weekend (Friday to Sunday) and weekday (Monday to Thursday)
mydata$DAY = revalue(mydata$DAY, c("S"="Weekend"))
mydata$DAY = revalue(mydata$DAY, c("F"="Weekend"))
mydata$DAY = revalue(mydata$DAY, c("M"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("T"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("W"="Weekday"))
mydata$DAY = revalue(mydata$DAY, c("Th"="Weekday"))
head(mydata)

# Transform the weekday = 0, weekend = 1
mydata$DAY=revalue(mydata$DAY, c("Weekday"="0"))
mydata$DAY=revalue(mydata$DAY, c("Weekend"="1")) 
head(mydata)

# declare variables
day = mydata$DAY
hours = mydata$HOURS
mail = mydata$MAIL
cert = mydata$CERT
acc = mydata$ACC
change = mydata$CHANGE
check = mydata$CHECK
misc = mydata$MISC
tickets = mydata$TICKETS

library(dummies)
library(caret)
library(boot)
library(leaps)

# a) bulid the logistic regression model using all data.
fit = glm(day ~ hours+mail+cert+acc+change+check+misc+tickets,data=mydata,family = binomial())
summary(fit) 
#According to z-test, hours,mail,cert,change,check are not significant(>0.05) but acc,misc,tickets are significant(<0.05).

# b) build a base model by using hours
base = glm(day~hours,data = mydata,family = binomial())
summary(base) #AIC 64.439

# b) forward stepwise by AIC value
m1 = step(base, scope=list(upper=fit, lower=~1), direction="forward", trace=F)
summary(m1) #forward AIC= 57.618

# b) backward stepwise by AIC value
m2 = step(fit, direction="backward", trace=F)
summary(m2) #backward AIC = 56.039 (better model)

# c) coefficients of x variables
summary(m2)
exp(coef(m2)) #compute coefficients to analyze change in odds for changes in x # convert the model to the original scale.



# We can interpret coefficients of exp(coef(m2)) like this below.

#e.g)
#The coefficient for mail= 0.0004114 which is interpreted as the expected 
#change in log odds for a one-unit increase in the mail score. 
#The odds ratio can be calculated by exponentiating this value to get 1.000411481
#which means we expect to see about 00.0411481% increase in the odds of being in an weekends, for a one-unit increase in mail score.
#Other x variables can be interpreted equally.

# We can use the confint function to obtain confidence intervals for the coefficient estimates.
# Note that for logistic models, confidence intervals are based on the profiled log-likelihood function.  
confint(m2) #95% CI for the codfficients
exp(confint(m2)) # 95% CI for exp(coefficients), that is change in odds

#criteria of cook'd: 4/n=4/52=0.0769=7.69*10^-2=7.69e-02
influence.measures(m2)
#index[3,9,18,29,31] are influence points
