setwd('C:/users/mg/Desktop/Data Analytics/NaiveBayes/')
getwd()

#data = read.table("iris.data", header=F, sep=",")
#head(data)

## 1-1. Load data and library for Hold-out evaluation##
#install.packages('naivebayes', dependencies = TRUE)
library(naivebayes)
data(iris)
head(iris)
names(iris)
ind_iris <- sample(1:nrow(iris), size = round(0.3 * nrow(iris))) #30% as testing
iris_train <- iris[-ind_iris, ] #70% of whole data
iris_test <- iris[ind_iris, ] #30% of whole data

## 1-2. Build Models and make predictions ##
nb_iris <- naive_bayes(Species ~., iris_train)
pred=predict(nb_iris, iris_test)
head(predict(nb_iris, iris_test, type = "prob"))

## 1-3. Evaluate performance ##
#install.packages('Metrics', dependencies = TRUE)
library(Metrics)
accuracy(iris_test[,5], pred) # (Actual data, predictions)

## 2-1. Load data and library for N-fold cross validation ##
#install.packages('caret', dependencies = TRUE)
library(caret)
head(iris)
names(iris)

## 2-2. Set features and labels ##
x = iris[,-5]
y = iris$Species

## 2-3. Build the model with 10-Folds cross validation ##
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10),na.action=na.pass)

## 2-4. Make the predictions ##
predict(model$finalModel,x)

## 2-5. Get accuracy ##
print(model)
