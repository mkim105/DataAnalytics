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


####################여기서부터는 예로 invmpg를 기준으로 잡고 models를 만들어볼 것##############
invmpg = 1/mpg

# plot을 보면 displacement는 positive 선형.
# horsepower도 variance가 크긴한데 positive
# wt는 positive 선형.
# acc는 negative에 큰 variance를 가지는것처럼보임. 
plot(displacement, invmpg)
plot(hp, invmpg)
plot(wt, invmpg)
plot(acc, invmpg)
# correlation을 보면 mpg와의 관계를 비교했을 때, hp와 acc는 약간의 선형관계가 보이고 
#displacement와 weight는 어느정도 강한 선형관계가 보임.
cor(invmpg, mydata[c(1,7:10)])



################################# Build models from here #################################
# only numericals, inv(y) tansformation
# Forward Selection, Backward Elimination, Best Subset, Stepwise를 모두 이용해서 best models를 찾아야함. (W6_01_15p)

# with 95% confidence or significance level. 

#### 이 모델은 using only numericals, y transformed(invmpg)의 모델이다. ####


################################# 1. Feature Selections #################################

# Backward Elimination by manually drop x based on p-value
m1 = glm(invmpg~displacement+hp+wt+acc)
summary(m1) # 1. hp(p-value = 0.9024 > 0.05) 제거
m1 = glm(invmpg~displacement+wt+acc) # 최종 모델
summary(m1) # 3. 이제 모든 x variables의 p-value가 0.05보다 작음.

# Backward Elimination by step() based on AIC 

m2 = glm(invmpg~displacement+hp+wt+acc)
summary(m2) # AIC:-2760.3
m2 = step(m2, direction = "backward", trace=TRUE) # 최종 모델, AIC가 작을수록 좋음 AIC:-2762.27

# Forward Selection by step()
base = glm(invmpg~displacement)
full = glm(invmpg~displacement+hp+wt+acc)
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델
summary(m3)

# Both direction by step()
m4 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m4)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=invmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="adjr2") #adjr2 > 11번째가 가장큼. hp만 false.
m5 = glm(invmpg~displacement+wt+acc) # The corresponding model is to use displacement,wt,acc (equal to index[11])
summary(m5)

#A small value of Cp means that the model is relatively precise.
leaps(y=invmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="Cp") #Cp > 11번째가 가장큼. hp만 false. 따라서 m5와 동일

# m1 : invmpg~displacement+wt+acc
# m2 : invmpg~displacement+wt+acc
# m3 : invmpg~displacement+wt+acc
# m4 : invmpg~displacement+wt+acc
# m5 : invmpg~displacement+wt+acc

# 여기서 m1~m5는 모두 같은 모델을 가지므로 m1하나로만 테스트를 하겠다.

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# We do not have adjR2 and F-test results.
# It is because we use glm to build the linear regression models.
# So as long as x variable has small p-value in t-test, it is satisfied. < 이것은 이미 모델을 만들면서 해결하였음.
# AdjR2같은 경우에는 필요하다면 lm()을 이용하여 구해야함.

################################# 3. VIF (To solve multi-collinearity problem) #################################
# VIF가 4보다크다면 문제가 있는거임.
library(car)
vif(m1)
#m1에서 displacement, wt의 vif가 4를 넘어가므로 가장큰 displacement를 제거한다.
m6 = glm(invmpg~wt+acc)
vif(m6) #vif을 만족하는 모델 m6를 얻었다.

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

#m6의 residual analysis
res=rstandard(m6)
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.1367 > 0.05 Normal distribution을 따르지않으므로 해당 모델은 적합하지않다.
