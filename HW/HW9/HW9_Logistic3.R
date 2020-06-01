


######################################
######################################
######################################
# Training the multinomial model
multinom_model <- multinom(mydata$V6 ~ ., data = mydata)

# Just like binary logistic regression model we need to convert the coefficients
# to odds by taking the exponential of the coefficients.
exp(coef(multinom_model))
# The predicted values are saved as fitted.values in the model object. 
# Let¡¯s see the top 6 observations.
head(round(fitted(multinom_model), 2))
# The multinomial regression predicts the probabily of a particular observation
# to be part of the said level. This is what we are seeing in the above table. 
# Columns represent the classification levels and rows represent the observations. 
# This means that the first six observation are classified as '3' except index[5]

# Predicting and Validating the model
# To validate the model we will be looking at accuracy of the model. 
# This accuracy can be calculated from the classification table.

# Predicting the values for train dataset
train$v6predicted <- predict(multinom_model, newdata = train, "class")

# Building classification table
table <- table(train$V6, train$v6predicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(table))/sum(table))*100,2) # accuracy = 53.27%

print(multinom_model)
accuracy(train$v6,train$v6predicted)
"""
# Predicting the v6 based upon the features of mydata
# in the test dataset.

# Predicting the class for test dataset
test$v6predicted <- predict(multinom_model, newdata = test, "class")

# Building classification table
table <- table(test$v6, test$v6predicted)
table
"""