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
plot(dis, logmpg)
plot(hp, logmpg) 
plot(wt, logmpg)
plot(acc, logmpg)
#logmpg~hp 관계가 어쩌면 negative, polynomial 일수도? <<근데 logmpg~hp관계가 polynomial이 맞다면 onlynumerical에서
#hp를 high-order term이용해서 바꿔야함.
#dis,wt와의 관계는 직선,-형으로 보이며 acc와의 관계는 variance가 큰 + 관계로보인다.


################################# Build models from here #################################

# with 95% confidence or significance level. 

#### 이 모델은 using all variables + y transformed(logmpg)의 모델이다. #### 
################################# 1. Feature Selections #################################

# Backward Elimination by manually drop x based on p-value
###logmpg~hp관계가 polynomial인지, 맞다면 어떤건지 확인하기###
hp2 = hp*hp
hp3 = hp*hp2
loghp = log(hp)
sqrthp = sqrt(hp)

homodel = glm(logmpg ~ dis+ hp + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(homodel)
homodel = glm(logmpg ~ dis+ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(homodel)
homodel = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + hp3 + loghp + sqrthp, data=mydata)
summary(homodel)
homodel = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + loghp + sqrthp, data=mydata)
summary(homodel)
homodel = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2 + sqrthp, data=mydata)
summary(homodel)
homodel = glm(logmpg ~ hp + wt + as.factor(cylinders)+ as.factor(year) + as.factor(org) + hp2, data=mydata)
summary(homodel)

# Backward Elimination by step() based on AIC 
m1 = glm(logmpg ~ dis + hp + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)
summary(m1) # AIC: -624.27
m1 = step(m1, direction = "backward", trace=TRUE) # 최종 모델, AIC가 작을수록 좋음 AIC:-627.51
summary(m1)

# Forward Selection by step()
base = glm(logmpg~dis)
full = glm(logmpg~dis+ hp + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)
m2 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델
summary(m2)

# Both direction by step()
m3 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m3)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=logmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="adjr2") #adjr2 > 55번째가 가장큼. 전부 TRUE
m4 = full
summary(m4)

#A small value of Cp means that the model is relatively precise.
leaps(y=logmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="Cp") # 55번째가 가장큼. 전부 TRUE
m5 = m4

# m1 = glm(logmpg ~ wt + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# m2 = glm(logmpg ~ dis + as.factor(year) + wt + as.factor(cylinders) + as.factor(org) + acc)
# m3 = glm(logmpg ~ as.factor(year) + wt + as.factor(cylinders) + as.factor(org))
# m4 = glm(logmpg ~ dis + hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# m5 = glm(logmpg ~ dis + hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
# 따라서 모델 m1,m2,m3,4를 구함(m4와 m5는 동일하기에)

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# We do not have adjR2 and F-test results.
# It is because we use glm to build the linear regression models.
# So as long as x variable has small p-value in t-test, it is satisfied. < 이것은 이미 모델을 만들면서 해결하였음.
# AdjR2같은 경우에는 필요하다면 lm()을 이용하여 구해야함.

################################# 3. VIF (To solve multi-collinearity problem) #################################
# VIF가 4보다크다면 문제가 있는거임.

library(car)
vif(m1) # cylinders의 gvif가 8을 넘으므로 cylinders를 제거한다.
m5 = glm(logmpg ~ wt + as.factor(year) + as.factor(org), data=mydata)
vif(m5)

vif(m2) # dis=22를 제거한다.
m6 = glm(logmpg ~ as.factor(year) + wt + as.factor(cylinders) + as.factor(org) + acc)
vif(m6) # cylinders를 제거한다.
m6 = glm(logmpg ~ as.factor(year) + wt + as.factor(org) + acc)
vif(m6)

vif(m3) # cylinders를 제거한다.
m7 = glm(logmpg ~ as.factor(year) + wt + as.factor(org))
vif(m7)

vif(m4) # dis를 제거한다.
m8 = glm(logmpg ~ hp + wt + acc + as.factor(cylinders) + as.factor(year) + as.factor(org), data = mydata)
vif(m8) # cylinders를 제거한다.
m8 = glm(logmpg ~ hp + wt + acc + as.factor(year) + as.factor(org), data = mydata)
vif(m8)

# 여기서 최종 모델은
# m5 = glm(logmpg ~ wt + as.factor(year) + as.factor(org), data=mydata) 
# m6 = glm(logmpg ~ as.factor(year) + wt + as.factor(org) + acc)
# m7 = glm(logmpg ~ as.factor(year) + wt + as.factor(org))
# m8 = glm(logmpg ~ hp + wt + acc + as.factor(year) + as.factor(org), data = mydata)

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

#m5의 residual analysis
res=rstandard(m5)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.001245 > 0.05 Normal distribution을 따르지않는다.

#m6의 residual analysis
res=rstandard(m6)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.002172 > 0.05 Normal distribution을 따르지않는다.

#m7의 residual analysis
res=rstandard(m7)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.001245 > 0.05 Normal distribution을 따르지않는다.

#m8의 residual analysis
res=rstandard(m8)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.001379 > 0.05 Normal distribution을 따르지않는다.

# 따라서 이중에 적합한 모델은 없다.


# Interaction terms을 적용한 모델도 나중에 해야해
