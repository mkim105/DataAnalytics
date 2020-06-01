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
### 2. Build model by using Naive Bayes ###

## 2-1. Load data and library for N-fold cross validation ##
#install.packages('caret', dependencies = TRUE)
setwd('C:/users/mg/Desktop/Data Analytics/HW/HW9/')
getwd()
data = read.table("tae.data", header=F, sep=",")
library(caret)
head(data)
names(data)
set.seed(1234)

# declaration of variables
ntv = data$V1    # Binary
instr = data$V2  # Categorical
crse = data$V3   # Categorical
smster = data$V4 # Binary
cs = data$V5     # Numerical
ca = data$V6     # Categorical # Label

# preprocessing to improve a model.
# For any numerical features, it is better to convert them to nominal
data[,5] = cut(data[,5], 4) # cut function to create 3 groups

## 2-2. Set features and labels ##
x = data[,-6] # exclude v6
y = ca
y = factor(y)

# since data size is small, i will use N-fold cross validation
## 2-3. Build the model with 10-Folds cross validation ##
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10),na.action=na.pass)

## 2-4. Make the predictions ##
predict(model$finalModel,x)

## 2-5. Get accuracy ##
print(model) #accuracy = 0.5411905

