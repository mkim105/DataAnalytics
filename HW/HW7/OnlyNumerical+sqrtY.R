setwd('C:/users/mg/Desktop/Data Analytics/HW/HW7/')
getwd()

data = read.table("HW7_AutoMPG.csv", header=T, sep=',')
head(data)

##############################################이 안의 과정은 missing value 찾기#######################################################

# 강제로 지워버리는 independent variables는 없다.
library(dummies)
mydata = data

# Multi-valued discrete values can be a categorical values.
mydata = dummy.data.frame(mydata,names=c("cylinders")) #gas_type을 2개의 dummy variables로 만든다.(gas_typeE10 and gas_typeSP98)
head(mydata)

mydata = dummy.data.frame(mydata,names=c("year"))
head(mydata)

mydata = dummy.data.frame(mydata,names=c("origin"))
head(mydata)

summary(mydata)

# Group Averages Over Level Combinations Of Factors 
mydata$horsepower = ifelse(is.na(mydata$horsepower), ave(data$horsepower, FUN = function(x) mean(x, na.rm=T)), mydata$horsepower)

head(mydata)

summary(mydata)

##############################################이 안의 과정은 missing value 찾기 구간##############################################

# Make Multiple Linear Regression Model.

# y variable = mpg, x variables = other 7 variables.

nrow(mydata) # data size = 398, so we should select N-fold evaluation.

# 1. examine the relationship between x and y.
# colnames(mydata)

# cylinders, year, org는 dummy variables == binary variables이므로 correlations를 해석하기 
#어려우므로 이들을 제외하고 correlations를 비교한다.
# 변수선언
mpg = mydata$mpg

displacement = mydata$displacement
hp = mydata$horsepower
wt = mydata$weight
acc = mydata$acceleration

#cylinders = mydata[c(1, 2:6)]
cylinders = mydata[c(2:6)]

#year = mydata[c(1, 11:22)]
year = mydata[c(11:22)]

#org = mydata[c(1, 24:26)]
org = mydata[c(24:26)]

# Dummy variables을 어떻게 lm 모델에 한꺼번에 넣을수있지?


####################여기서부터는 예로 sqrtmpg를 기준으로 잡고 models를 만들어볼 것##############
sqrtmpg = sqrt(mpg)

# plot을 보면 displacement,wt는 negative한 선형패턴이보이고 
# hp는 네거티브하나 큰 variance를 가지고, acc는 positive한 선형패턴이보임.
plot(displacement, sqrtmpg)
plot(hp, sqrtmpg)
plot(wt, sqrtmpg)
plot(acc, sqrtmpg)
# correlation을 보면 sqrtmpg와의 관계를 비교했을 때, hp와 acc는 약간의 선형관계가 보이고 
#displacement와 weight는 어느정도 강한 선형관계가 보임.
cor(sqrtmpg, mydata[c(1,7:10)])



################################# Build models from here #################################
# only numericals + sqrt(y)
# Forward Selection, Backward Elimination, Best Subset, Stepwise를 모두 이용해서 best models를 찾아야함. (W6_01_15p)

# with 95% confidence or significance level. 

#### 이 모델은 using only numericals, y transformed(sqrtmpg)의 모델이다. #### 
################################# 1. Feature Selections #################################
# (dummy variables은 제외한 상태로 시작)

# Backward Elimination by manually drop x based on p-value
m1 = glm(sqrtmpg~displacement+hp+wt+acc)
summary(m1) # 1. hp(p-value = 0.5078 > 0.05) 제거
m1 = glm(sqrtmpg~displacement+wt+acc)
summary(m1) # 2. acc(p-value = 0.0676 > 0.05) 제거
m1 = glm(sqrtmpg~displacement+wt) # 최종 모델  
summary(m1) # 3. 이제 모든 x variables의 p-value가 0.05보다 작음.

# Backward Elimination by step() based on AIC 

m2 = glm(sqrtmpg~displacement+hp+wt+acc)
summary(m2) # AIC:420.5
m2 = step(m2, direction = "backward", trace=TRUE) # 최종 모델, AIC가 작을수록 좋음 AIC:418.94

# Forward Selection by step()
base = glm(sqrtmpg~displacement)
full = glm(sqrtmpg~displacement+hp+wt+acc)
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델
summary(m3)

# Both direction by step()
m4 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m4)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=sqrtmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="adjr2") #adjr2 > 11번째가 가장큼. hp만 false.
m5 = glm(sqrtmpg~displacement+wt+acc) # The corresponding model is to use displacement,wt,acc (equal to index[11])
summary(m5)

#A small value of Cp means that the model is relatively precise.
leaps(y=sqrtmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="Cp") #Cp > 11번째가 가장큼. hp만 false. 따라서 m5와 동일

# m1 : sqrtmpg~displacement+wt
# m2 : sqrtmpg~displacement+wt+acc
# m3 : sqrtmpg~displacement+wt+acc
# m4 : sqrtmpg~displacement+wt+acc
# m5 : sqrtmpg~displacement+wt+acc

# 여기서 m2~m5는 모두 같은 모델을 가지므로 m2하나로만 테스트를 하겠다.

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# We do not have adjR2 and F-test results.
# It is because we use glm to build the linear regression models.
# So as long as x variable has small p-value in t-test, it is satisfied. < 이것은 이미 모델을 만들면서 해결하였음.
# AdjR2같은 경우에는 필요하다면 lm()을 이용하여 구해야함.

################################# 3. VIF (To solve multi-collinearity problem) #################################
# VIF가 4보다크다면 문제가 있는거임.
library(car)
vif(m1)
#m1에서 displacement와 wt의 vif가 4를 넘어가므로, 각각 m6와 m7을 만든다.
m6 = glm(sqrtmpg~wt)
m7 = glm(sqrtmpg~displacement)

vif(m2)
#m2에서 displacement와 wt의 vif가 4를 넘어가므로 가장 큰 displacement를 제거한다. 
m8 = glm(sqrtmpg~wt+acc)
vif(m8)

# 그러면 여기서 m6,m7,m8이 나옴.

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

#m6의 residual analysis
res=rstandard(m6)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.0007163 > 0.05 Normal distribution을 따르지않으므로 탈락.

#m7의 residual analysis
res=rstandard(m7)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.000565 > 0.05 Normal distribution를 따르지않으므로 탈ㄹ

#m8의 residual analysis
res=rstandard(m8)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.02399 < 0.05 Normal distribution을 따르지않으므로 탈ㄹ

#따라서 residual analysis를 통과한 모델은 없다.

