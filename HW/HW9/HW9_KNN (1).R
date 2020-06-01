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

v1 = data$V1  
v2 = data$V2
v3 = data$V3
v4 = data$V4
v5 = data$V5
v6 = data$V6  #y variable.

# KNN classification using N-fold method 
library(caret)
library(Metrics)
library(dummies)
library(class)

# pre-processing

# convert categorical to dummy variables (v2, v3)
data_pped = dummy.data.frame(data,names = c("V2"))
data_pped = dummy.data.frame(data_pped,names = c("V3"))

myvars<-c("V1","V21","V22","V23","V24","V25","V26","V27","V28","V29","V210","V211","V212","V213","V214","V215","V216","V217","V218","V219", "V220","V221","V222",
          "V223","V224","V225","V31","V32","V33","V34","V35","V36","V37","V38","V39","V310","V311","V312","V313","V314","V315","V316","V317","V318","V319",
          "V320","V321","V322","V323","V324","V325","V326","V4","V5")
head(data_pped)

# Normailization
num.vars<-sapply(data_pped[,myvars],is.numeric)
num.vars
data_pped[num.vars]<-lapply(data_pped[,num.vars],scale)

# declare x,y variables and build model

x<-data_pped[myvars]
x
y<-as.factor(v6)
y

# since data size is small, i will use N-fold cross validation
m1=train(x,y,'knn',trControl = trainControl(method = 'cv',number = 10),tuneGrid = expand.grid(k=1:10))
predict(m1$finalModel,x)
print(m1) 
#k=1 > Accuracy:0.6872, k=2 > Accuracy:0.4953, k=8 > Accuracy:0.4904
