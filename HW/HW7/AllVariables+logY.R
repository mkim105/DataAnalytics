getwd()
setwd('C:/users/mg/Desktop/Data Analytics/HW/HW7/')
mydata=read.table('HW7_AutoMPG.csv',header=T,',')

# In this case, I used all variables.

######################################################################################################################
# Purpose of this section : Change discrete values as categorical values. Solve missing values problem.

# Multi-valued discrete values can be a categorical values.
#all data is numerical
summary(mydata) #it tells if missing values exist or not

# horsepower missing data preprocessing
mydata$horsepower = ifelse(is.na(mydata$horsepower), ave(data$horsepower, FUN = function(x) mean(x, na.rm=T)), mydata$horsepower)

summary(mydata)
######################################################################################################################
# Purpose of this section : Variable declarations and examine relationships to create models.
# Make Multiple Linear Regression Model.
# y variable = mpg, x variables = other 7 variables.

nrow(mydata) # data size = 398, so we should select N-fold evaluation.

# 1. examine the relationship between x and y.

mpg = mydata$mpg

dis = mydata$displacement
hp = mydata$horsepower
wt = mydata$weight
acc = mydata$acceleration
cylinders=mydata$cylinders
year = mydata$year
org = mydata$origin

# Because cylinders, year, org are dummy variables == binary variables,
# It is difficult to interpret the correlations, so we compare the correlations except these.

cor(mydata[-c(2,7,8)])
plot(mydata[-c(2,7,8)])

# I thought that I could increase the correlation more, so I proceeded to transform Y.
logmpg = log(mpg)


cor(logmpg, mydata[-c(2,7,8)])
attach(mtcars)
par(mfrow=c(2,2))
plot(dis, logmpg)
plot(hp, logmpg) 
plot(wt, logmpg)
plot(acc, logmpg)
#The plot shows displacement, wt has a negative linear pattern and hp is negative, but possibly polynomial.
#acc is positive and has a linear pattern with a large variance.

################################# Build models from here #################################

# Find the best models using both Forward Selection, Backward Elimination, Best Subset, and Stepwise.

# with 95% confidence or significance level. 

################################# 1. Feature Selections #################################

# Backward Elimination by manually drop x based on p-value
hp2 = hp*hp
hp3 = hp*hp2
loghp = log(hp)
sqrthp = sqrt(hp)

m0 = glm(logmpg ~ dis+ hp + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(m0)
m0 = glm(logmpg ~ dis+ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(m0)
m0 = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(m0)
m0 = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + loghp + sqrthp, data=mydata)
summary(m0)
m0 = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + sqrthp, data=mydata)
summary(m0)
m0 = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2, data=mydata)
summary(m0)
# I found that the relationship between logmpg and hp is a quadratic from this.
# So now on i will use hp2 as new variable.

# Backward Elimination by step() based on AIC 
m1 = glm(logmpg ~ dis + hp + wt + acc + hp2 + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)
summary(m1) # AIC: -629.18
m1 = step(m1, direction = "backward", trace=TRUE) # AIC:-632.27
summary(m1)

# Forward Selection by step()
base = glm(logmpg~dis)
full = glm(logmpg~dis+ hp + wt + acc + hp2 + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)
m2 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F)
summary(m2)

# Both direction by step()
m3 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m3)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=logmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="adjr2") #adjr2 index[55] is the biggest.
m4 = full
summary(m4)

#A small value of Cp means that the model is relatively precise.
leaps(y=logmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="Cp") # index[55] is the biggest.
m5 = m4

# m1 = glm(formula = logmpg ~ hp + wt + hp2 + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# m2 = glm(formula = logmpg ~ dis + as.factor(year) + wt + as.factor(cylinders) + as.factor(org) + acc)
# m3 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(cylinders) + as.factor(org))
# m4 = glm(logmpg ~ dis + hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# m5 = glm(logmpg ~ dis + hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# Therefore, we obtain models m1, m2, m3, m4, and m5 (m4 and m5 are the same so i use m4 only.).

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# We do not have adjR2 and F-test results.
# It is because we use glm to build the linear regression models.

################################# 3. VIF (To solve multi-collinearity problem) #################################

library(car)
vif(m1) # Remove hp2 because gvif > 4
m6 = glm(formula = logmpg ~ hp + wt + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
vif(m6) # Remove cylinders because of same reason
m6 = glm(formula = logmpg ~ hp + wt + as.factor(year) + as.factor(org), data = mydata)
vif(m6) # O.K

vif(m2) # Remove dis
m7 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(cylinders) + as.factor(org) + acc)
vif(m7) # Remove cylinders
m7 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(org) + acc)
vif(m7) # O.K

vif(m3) # Remove cylinders
m8 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(org))
vif(m8) # O.K

vif(m4) # Remove dis
m9 = glm(logmpg ~ hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
vif(m9) # Remove cylinders
m9 = glm(logmpg ~ hp + wt + acc + as.factor(year) + as.factor(org), data = mydata)
vif(m9)

# m6 = glm(formula = logmpg ~ hp + wt + as.factor(year) + as.factor(org), data = mydata) 
# m7 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(org) + acc)
# m8 = glm(formula = logmpg ~ as.factor(year) + wt + as.factor(org))
# m9 = glm(logmpg ~ hp + wt + acc + as.factor(year) + as.factor(org), data = mydata)

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

#m6 residual analysis
res=rstandard(m6)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value > 0.05, Not a Normal distribution

#m7 residual analysis
res=rstandard(m7)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value > 0.05, Not a Normal distribution

#m8 residual analysis
res=rstandard(m8)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value > 0.05, Not aNormal distribution

#m9 residual analysis
res=rstandard(m9)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value > 0.05 Not a Normal distribution

# Therefore, there is no suitable model.
