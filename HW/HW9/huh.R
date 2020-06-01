"""
Objective:
여름/일반학기 동안의 TA(151명)의 teaching performance에 대한 데이터이다.
점수는 3 개의 대략 동일한 크기의 카테고리 ( "낮음", "중간"및 "높음")로 분류되어 클래스 변수를 형성했습니다.
따라서 x variables를 가지고 y variable(TA의 teaching performance)를 예상할 수 있는가?
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

data = read.table("tae.data", header=F, sep=",")
head(data)
str(data)
set.seed(1234)
# declaration of variables
ntv = data$V1    # Binary
instr = data$V2  # Categorical
crse = data$V3   # Categorical
smster = data$V4 # Binary
cs = data$V5     # Numerical
ca = data$V6     # Categorical # Label

gc = data[sample(nrow(data)),] # sample from data
select.data = sample(1:nrow(gc), 0.8*nrow(gc))
train.gc = gc[select.data,]
test.gc = gc[-select.data,]
train.def <- gc$V6[select.data]
test.def <- gc$V6[-select.data]
test.def <- factor(test.def)

### 1. Build model by using Decision Tree ###
#install.packages("rpart")
library(rpart) # using Gini index
library(caret)
# grow the tree using Hold-out evaluation
fit = rpart(V6~V1+V2+V3+V4+V5, method="class", data=train.gc)

"""
# grow the tree using N-fold cross validation
#library(caret)
#fit <- train(V6~V1+V2+V3+V4+V5, data=data, method="rpart", 
trControl=trainControl(method="cv", number=10), tuneLength=10,
parms=list(split='information'))
#print(fit)
"""

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit, cex=0.7)


# plot tree
plot(fit, uniform=TRUE, main="Classfication Tree");text(fit, use.n=TRUE, all=TRUE, cex=.8) # cex:size of letter

# prune the tree
pfit<-prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit,uniform=TRUE,main="Pruned Classification Tree");text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# evaluate the model based on the test set
pred_fit <- predict(fit, newdata=test.gc, type="class")
confusionMatrix(pred_fit, test.def)

pred_pfit <- predict(pfit, newdata=test.gc, type="class")
confusionMatrix(pred_pfit, test.def)

printcp(fit)
plotcp(fit)
summary(fit)

