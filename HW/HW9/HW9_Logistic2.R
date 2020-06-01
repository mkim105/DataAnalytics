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
mydata=read.table('tae.data',header=F,',')
head(mydata)
str(mydata)

# building multiclass regression model
library(caret)
index <- createDataPartition(mydata$V6, p = .70, list = FALSE)
train <- mydata[index,] #70% of whole data
test <- mydata[-index,] #30% of whole data

# To train the model we will be using multinom function from nnet are package. 
# Once the model is trained then we will use the summary() function 
# to check the model coefficients.
require(nnet)
# Training the multinomial model
myvars=c("V1","V2","V3","V4","V5")
myvars
x<-mydata[myvars]
x
y<-mydata$V6
y = factor(y)
multinom_model <- train(x,y,'multinom',trControl=trainControl(method='cv',number=10))
print(multinom_model) # accuracy = 0.5169

# Checking the model
summary(multinom_model) #AIC:315


###########Feature selection###########
x_base<-mydata[c("V1")]
x_base
multinom_model_base <- train(x_base,y,'multinom',trControl=trainControl(method='cv',number=10))
#forward stepwise by AIC value
forward = step(multinom(mydata$V6 ~., mydata), direction="forward")
summary(forward) #forward AIC= 315.2805

#backward stepwise by AIC value
backward = step(multinom(mydata$V6 ~., mydata), direction="backward")
summary(backward) #backward AIC = 309.0044  - better model!
print(backward) # V6 ~ V1 + V3 + V4

# building multiclass regression model using feature selection model(backward)

# Training the multinomial model
myvars2=c("V1", "V3", "V4")
myvars2
x2<-mydata[myvars]
x2
multinom_model2 <- train(x2,y,'multinom',trControl=trainControl(method='cv',number=10))
print(multinom_model2) # accuracy = 0.5434

# Checking the model
summary(multinom_model2) #AIC:315

# Conclusion : multinom_model2 is better than multinom_model. (Accuracy: 0.5169 > 0.5434)

