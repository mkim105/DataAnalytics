getwd()
setwd('C:/users/mg/Desktop/Data Analytics/HW/HW7/')
mydata=read.table('HW7_AutoMPG.csv',header=T,',')

# In this case, I used all variables and interaction terms

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
hp2 = hp*hp
# Interaction terms variables.

# Backward Elimination by step() based on AIC
model = glm(logmpg ~ (dis+hp+wt+acc+hp2+as.factor(cylinders)+ as.factor(year) + as.factor(org))^2, data=mydata)
summary(model) # AIC:-692.07
model = step(model, direction = "backward", trace=TRUE)
summary(model) # AIC:-729.25

# Both direction by step()
full = glm(logmpg ~ (dis+hp+wt+acc+hp2+as.factor(cylinders)+ as.factor(year) + as.factor(org))^2, data=mydata)
base = glm(logmpg ~ dis)
model2 = step(base, scope=list(upper=model, lower=~1), direction="both", trace=F)
summary(model2) # Since -692 > -655, discard this model

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# We do not have adjR2 and F-test results.
# It is because we use glm to build the linear regression models.

################################# 3. VIF (To solve multi-collinearity problem) #################################

vif(model) # Error in vif.default(model) : there are aliased coefficients in the model

################################# 4. Residual Analysis (To validate models is qualified or not) #################################

# Residual analysis of m6
res=rstandard(model)
attach(mtcars)
par(mfrow=c(3,3))
#1)	Validate the constant variance
plot(fitted(model),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')

#2)	Validate the linearity relationship
plot(dis, res, main="dis vs residuals plot")
abline(a=0, b=0, col='red')
plot(hp, res, main="hp vs residuals plot")
abline(a=0, b=0, col='red')
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
plot(hp2, res, main="hp2 vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(cylinders), res, main="as.factor(cylinders) vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(year), res, main="as.factor(year) vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(org), res, main="as.factor(org) vs residuals plot")
abline(a=0, b=0, col='red')

#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.4787 > 0.05 : Normal distribution


#4) Identify potential outliers and (influential points) 

# Identify potential outliers and (influential points) about model.
#The criteria is 4/n = 4/398 = 0.01005025. If cook.d is larger than 4/n, it is influential points.
library(stats)
#influence.measures(model)
cooks.distance(model)
sort(cooks.distance(model), TRUE)[46] #1 to 46 are influential points, but use only until the 20th.
sort(cooks.distance(model), TRUE)

# Create new data by removing influential points
ndata = mydata[-c(334,317,77,375,388,337,300,20,390,24,301,361,210,167,102,203,324,123,211,157),]

nmpg = ndata$mpg
nlogmpg = log(nmpg)
ndis = ndata$displacement
nhp = ndata$horsepower
nwt = ndata$weight
nacc = ndata$acceleration
nhp2 = nhp*nhp
ncy = ndata$cylinders
nyr = ndata$year
norg = ndata$origin

# Build model based on the model_new
model_new = glm(nlogmpg ~ (ndis+nhp+nwt+nacc+nhp2+as.factor(ncy)+ as.factor(nyr) + as.factor(norg))^2, data=ndata)
summary(model_new) # AIC:-734.34
model_new = step(model_new, direction = "backward", trace=TRUE)
summary(model_new) # AIC:-787.51

################################# 5. Evaluate the model #################################

#5) Evaluate the model.
#install.packages('boot')
library('boot')
# N-fold cross validation

mse_model_new = cv.glm(ndata, model_new, K=5)$delta
mse_model_new # 0.2277421
