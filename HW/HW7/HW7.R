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

##############################################이 안의 과정은 missing value 찾기 구간#######################################################

# Make Multiple Linear Regression Model.

# y variable = mpg, x variables = other 7 variables.

nrow(mydata) # data size = 388, so we should select N-fold evaluation.

# 1. examine the relationship between x and y.
# colnames(mydata)

# cylinders, year, org는 dummy variables == binary variables이므로 correlations를 해석하기 어려우므로 이들을 제외하고 correlations를 비교한다.
# 변수선언
mpg = mydata$mpg

displacement = mydata$displacement
hp = mydata$horsepower
wt = mydata$weight
acc = mydata$acceleration

#cylinders = mydata[c(1, 2:6)]
cylinders = mydata[c(2:6)]
cy3 = mydata$cylinders3
cy4 = mydata$cylinders4
cy5 = mydata$cylinders5
cy6 = mydata$cylinders6
cy8 = mydata$cylinders8

#year = mydata[c(1, 11:22)]
year = mydata[c(11:22)]
year
#year = mydata$year70
#year = mydata$year82

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
plot(mpg,hp)
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

# plot을 보면 displacement,wt는 negative한 선형패턴이보이고 hp,acc는 positive한 선형패턴이보임.
plot(logmpg, displacement)
plot(logmpg, hp)
plot(logmpg, wt)
plot(logmpg, acc) 
# correlation을 보면 mpg와의 관계를 비교했을 때, hp와 acc는 약간의 선형관계가 보이고 displacement와 weight는 어느정도 강한 선형관계가 보임.
cor(logmpg, mydata[c(1,7:10)])



################################# Build models from here #################################
# 케이스는 (using all variable, only numericals, only use one numerical) + (y transformed and x transformed) + (transformed : x^2, x^3, log, inv, sqrt를 모두 고려해야함.
# Forward Selection, Backward Elimination, Best Subset, Stepwise를 모두 이용해서 best models를 찾아야함. (W6_01_15p)

# with 95% confidence or significance level. 

# 이 모델은 using only numericals, y transformed의 모델이다. 
################################# 1. Feature Selections #################################
# (dummy variables은 제외한 상태로 시작)

# Backward Elimination by manually drop x based on p-value
m1 = glm(mpg~displacement+hp+wt+acc)
summary(m1) # 1. hp(p-value = 0.5633 > 0.05) 제거
m1 = glm(mpg~displacement+wt+acc)
summary(m1) # 2. displacement(p-value = 0.0947 > 0.05) 제거
m1 = glm(mpg~wt+acc) # 최종 모델  
summary(m1) # 3. 이제 모든 x variables의 p-value가 0.05보다 작음.

# Backward Elimination by step() based on AIC 

m2 = glm(mpg~displacement+hp+wt+acc)
summary(m2) # AIC:2297.1
m2 = step(m2, direction = "backward", trace=TRUE) # 최종 모델, AIC가 작을수록 좋음 AIC:2295.48

# Forward Selection by step()
base = glm(mpg~displacement)
full = glm(mpg~displacement+hp+wt+acc)
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델

# Both direction by step()
m4 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)

# Best Subset selection by (Cp,r,adjr2 with least number of x variables)  (W6_01_37p)
#install.packages('leaps')
library(leaps)
leaps(y=mpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="adjr2") #adjr2 > 11번째가 가장큼. hp만 false.
m5 = glm(mpg~displacement+wt+acc) # The corresponding model is to use displacement,wt,acc (equal to index[11])
summary(m5)

# m1 : mpg~wt+acc
# m2 : mpg~displacement+wt+acc
# m3 : mpg~displacement+wt+acc
# m4 : mpg~displacement+wt+acc
# m5 : mpg~displacement+wt+acc

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
vif(m2)

# m2에서 다중공선성 문제가 발생되었으므로 displacement나 wt중에 하나를 지워야하는데, displacement를 지우면 m1과 같은 모델이 되므로 m1만을 남김.

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

res=rstandard(m1)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m1),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#1)과 2)의 상단 그래프 두개를 보면 증가패턴과 감소패턴의 그래프가 보인다.
#따라서 inv와 sqrt도 써봐야할듯하다. 
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
                  #p-value = 5.766e-06 < 0.05 ... normal distribution을 따르지않는 것 같은데,,,?
#4) Identify potential outliers
#plot을 보면 [-3,3]를 넘어가는 outliers를 볼수 있다.
#일단 1),2)에서 발견된 문제를 해결하고 4)를 다시 검사해보아야한다.


# dummy variables의 적용은 (W7_02_05p)
# Run 5-fold cross validation with cv.glm() (W7_01_10p)





