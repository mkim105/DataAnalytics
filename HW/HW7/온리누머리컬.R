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

#year


#org = mydata[c(1, 24:26)]
org = mydata[c(24:26)]

# Dummy variables을 어떻게 lm 모델에 한꺼번에 넣을수있지?

# 다양한 y transformations

mpg2 = mpg*mpg
mpg3 = mpg*mpg
logmpg = log(mpg)
invmpg = 1/mpg
sqrtmpg = sqrt(mpg)

# examine the correlations between mpg and x variables
cor(mpg,mydata[c(1,7:10)])
# mpg와 x variables을 비교했을 때, 상관성을 좀 더 증가시킬 수 있을 것 같아서 Y에 대한 transformation을 진행했다.
cor(mpg2,mydata[c(1,7:10)])
cor(mpg3,mydata[c(1,7:10)])
cor(logmpg,mydata[c(1,7:10)])
cor(invmpg,mydata[c(1,7:10)])
cor(sqrtmpg,mydata[c(1,7:10)])
# 위에서 비교해보니 logmpg, invmpg, sqrtmpg에서 correlation의 상승이 있었다. 하나씩 차례대로 만들어보면될듯?
# Y변수만 transformation 했으니 X변수를 transformation한 값도 모델로 써봐야하는건가?

####################여기서부터는 예로 logmpg를 기준으로 잡고 models를 만들어볼 것##############

# plot을 보면 displacement,wt는 negative한 선형패턴이보이고 
# hp는 negative하나 variance가 큰 상태로 보이고, acc는 positive하나 variance가 큰 선형패턴이보임.
plot(displacement, logmpg)
plot(hp, logmpg)
plot(wt, logmpg)
plot(acc, logmpg)
# correlation을 보면 mpg와의 관계를 비교했을 때, 
# hp는 +긴하나 polynomial 일수도 있음. acc는 약간의 선형관계가 보이고 
#displacement와 weight는 어느정도 강한 선형관계가 보임.
cor(logmpg, mydata[c(1,7:10)])

#hp가 polynomial일수도있으므로 hp에 대한 변수 추가로 선언.
hp2 = hp*hp
hp3 = hp*hp2
sqrthp = sqrt(hp)
loghp = log(hp)
################################# Build models from here #################################
# 케이스는 (using all variable, only numericals, only use one numerical) + 
# (y transformed and x transformed) + (transformed : x^2, x^3, log, inv, sqrt를 모두 고려해야함.
# Forward Selection, Backward Elimination, Best Subset, Stepwise를 모두 이용해서 best models를 찾아야함. (W6_01_15p)

# with 95% confidence or significance level. 

#### 이 모델은 using only numericals, y transformed(logmpg)의 모델이다. #### 
################################# 1. Feature Selections #################################
# (dummy variables은 제외한 상태로 시작)

# Backward Elimination by manually drop x based on p-value
m1 = glm(logmpg~displacement+hp+wt+acc+hp2+hp3+sqrthp+loghp)
summary(m1) # 1. acc(p-value = 0.3368 > 0.05) 제거
m1 = glm(logmpg~displacement+hp+wt+hp2+hp3+sqrthp+loghp)
summary(m1) # 2. loghp(p-value = 0.2171 > 0.05) 제거
m1 = glm(logmpg~displacement+hp+wt+hp2+hp3+sqrthp)
summary(m1) # 3. sqrthp(p-value = 0.4223 > 0.05) 제거
m1 = glm(logmpg~displacement+hp+wt+hp2+hp3)
summary(m1) # 4. wt(p-value = 0.1263 > 0.05)제거
m1 = glm(logmpg~displacement+hp+hp2+hp3)
summary(m1)

# Backward Elimination by step() based on AIC 

m2 = glm(logmpg~displacement+hp+wt+acc+hp2+hp3+sqrthp+loghp)
summary(m2) # AIC:-341.54
m2 = step(m2, direction = "backward", trace=TRUE) # 최종 모델, AIC가 작을수록 좋음 AIC:-344.39

# Forward Selection by step()
base = glm(logmpg~displacement)
full = glm(logmpg~displacement+hp+wt+acc+hp2+hp3+sqrthp+loghp)
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델
summary(m3)

# Both direction by step()
m4 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m4)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=logmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="adjr2") #adjr2 > 11번째가 가장큼. hp만 false.
m5 = glm(logmpg~displacement+wt+acc) # The corresponding model is to use displacement,wt,acc (equal to index[11])
summary(m5)

#A small value of Cp means that the model is relatively precise.
leaps(y=logmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="Cp") #Cp > 11번째가 가장큼. hp만 false. 따라서 m5와 동일

# m1 : logmpg~wt+acc
# m2 : logmpg~displacement+wt+acc
# m3 : logmpg~displacement+wt+acc
# m4 : logmpg~displacement+wt+acc
# m5 : logmpg~displacement+wt+acc

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
m6 = glm(logmpg~wt)
m7 = glm(logmpg~displacement)

vif(m2)
#m2에서 displacement와 wt의 vif가 4를 넘어가므로 가장 큰 displacement를 제거한다. 
m8 = glm(logmpg~wt+acc)
vif(m8)

# 그러면 여기서 m6,m7,m8이 나옴.

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

#m6의 residual analysis
res=rstandard(m6)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m6),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#1)과 2)의 상단 그래프 두개를 보면 아주 소량의 증가패턴과 감소패턴의 그래프가 보이나 무시해도될듯,
#따라서 inv와 sqrt도 써봐야하나?
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.1367 > 0.05 Normal distribution을 따른다.

#m7의 residual analysis
res=rstandard(m7)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m7),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#1)과 2)의 상단 그래프 두개를 보면 아주 소량의 증가패턴과 감소패턴의 그래프가 보이나 무시해도될듯,
#따라서 inv와 sqrt도 써봐야하나?
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.01563 > 0.05 Normal distribution (를따르지않는다.)

#m8의 residual analysis
res=rstandard(m8)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m8),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#1)과 2)의 상단 그래프 두개를 보면 아주 소량의 증가패턴과 감소패턴의 그래프가 보이나 무시해도될듯,
#따라서 inv와 sqrt도 써봐야하나?
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.81 > 0.05 Normal distribution을 따른다.

#따라서 m6,m8만 residual analysis를 통과했다.

#4) Identify potential outliers and (influential points) 
#plot을 보면 [-3,3]를 넘어가는 outliers를 볼수 있다.

#m6에 대한 Identify potential outliers and (influential points) 
#The criteria is 4/n = 4/398 = 0.01005025. If cook.d is larger than 4/n, it is influential points.
library(stats)
#influence.measures(m6)
cooks.distance(m6)
sort(cooks.distance(m6), TRUE)[21] #1~21번째까지는 influentil points이지만 20번째까지만 지운다.
sort(cooks.distance(m6), TRUE)
# Create new data by removing influential points
m6_newdata = mydata[-c(112,29,45,323,157,365,27,125,327,388,326,395,299,330,113,26,245,328,72,167),]
m6_newmpg = m6_newdata$mpg
m6_newlogmpg = m6_newmpg * m6_newmpg
m6_newwt = m6_newdata$weight
# Build model based on the m6_new
newm6 = glm(m6_newlogmpg~m6_newwt)

#m8에 대한 Identify potential outliers and (influential points) 
#The criteria is 4/n = 4/398 = 0.01005025. If cook.d is larger than 4/n, it is influential points.
library(stats)
#influence.measures(m6)
cooks.distance(m8)
sort(cooks.distance(m8), TRUE)[20] #1~21번째까지는 influentil points이다. 이것들 모두 지워도될듯.
sort(cooks.distance(m8), TRUE)
# Create new data by removing influential points
m8_newdata = mydata[-c(327,29,395,60,329,112,365,328,14,334,155,125,326,156,330,167,157,361,300,298),]
m8_newmpg = m8_newdata$mpg
m8_newlogmpg = m8_newmpg * m8_newmpg
m8_newwt = m8_newdata$weight
m8_newacc = m8_newdata$acceleration
# Build model based on the m6_new
newm8 = glm(m8_newlogmpg~m8_newwt+m8_newacc)

################################# End of Residual Analysis #################################


#정리하자면 여기서한것은 온리뉴머리컬 + 로그Y에 대한 분석이고 m6,m8의 모델을 뽑았음.

#5) Evaluate the model.
#포텐셜아웃라이어찾고 여기과정만 끝내면 여기는끝남.
#install.packages('boot')
library('boot')
# N-fold cross validation

mse6 = cv.glm(mydata, m6, K=5)$delta
mse8 = cv.glm(mydata, m8, K=5)$delta
mse_newm6 = cv.glm(m6_newdata, newm6, K=5)$delta
mse_newm8 = cv.glm(m8_newdata, newm8, K=5)$delta

mse6
mse8
mse_newm6 #에러가 너무 높음. 좋은 influential points를 찾지못했다.
mse_newm8 #에러가 너무 높음. 좋은 influential points를 찾지못했다.
##비교했을때 mse8이 에러가 가장 작으므로 m8이 가장 적합한모델이다. 

###
#m8의 influential points까지 찾은 모델(m8)에 대해서 high-order term을 적용?
#m8 = glm(formula = logmpg ~ wt + acc)인데
plot(wt, logmpg)
plot(acc, logmpg)
#의 결과 플롯에서 곡선은 보이지않으므로 high-order term을 적용할 필요는 없을 것 같다.
###







