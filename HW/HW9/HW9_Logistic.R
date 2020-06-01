"""
Objective:
This is the data on the teaching performance of TA (151 students) during the summer / general semester.
So, can we predict the y variable (teaching performance of TA) with x variables?
"""

"""
Attribute Information:
1. Whether of not the TA is a native English speaker (binary); 1=English speaker, 2=non-English speaker 
2. Course instructor (categorical, 25 categories) 
3. Course (categorical, 26 categories) 
4. Summer or regular semester (binary) 1=Summer, 2=Regular 
5. Class size (numerical) 
6. Class attribute (categorical) 1=Low, 2=Medium, 3=High

Number of Instances: 151

Number of Attributes: 6 (including the class attribute)

Missing Attribute Values: None
"""

setwd('C:/users/mg/Desktop/Data Analytics/HW/HW9/')
getwd()
data=read.table('tae.data',header=F,',')
head(data)
set.seed(1234)

library(dummies)
library(caret)
library(nnet)
library(boot)
library(leaps)
library(car)  
library(nnet)  

v1 = data$V1  
v2 = data$V2
v3 = data$V3
v4 = data$V4
v5 = data$V5
v6 = data$V6 

# bulid a logistic regression model using whole data, full model
myvars=c("V1","V2","V3","V4","V5")
myvars

x<-data[myvars]
x

y<-factor(v6)
y

#FULL model
fit = train(x,y, 'multinom',trControl = trainControl(method = 'cv',number=10))
print(fit)   #accuracy = 0.5353571
summary(fit) #AIC:315.2911

#stepwise v1 base 
x1<-data["V1"]
x1
base = train(x1,y, 'multinom',trControl = trainControl(method = 'cv',number=5))
base
summary(base) 

# Error
#forward stepwise by AIC value
m1 = step(base, scope=list(upper=fit, lower=~1), direction="", trace=F)
#m1 = step(multinom(v6 ~., data), direction="forward")
summary(m1) #forward AIC= 


#backward stepwise by AIC value
m2 = step(multinom(y ~., data), direction="backward")
summary(m2) #backward AIC = 8.069317  - better model!
print(m2)

# Error
cv.glm(fit,data = data,K=5)$delta
print(fit)



