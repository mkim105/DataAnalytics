setwd('C:/users/mg/Desktop/Data Analytics/HW/HW6')
getwd()

mydata = read.table("HW6_energytemp.txt", header=T)

energy = mydata$energy
tempd = mydata$temp

tempd2 = tempd^2;
tempd3 = tempd^3;

# a) Create a scatterplot.
plot(tempd, energy)
cor(tempd, energy)

# b) Fit the cubic model

#   a) Test the goodness of fit test(F-test).
#   b) Are all variables in the model significant?
fit = lm(energy ~ tempd+tempd2+tempd3, data=mydata)
summary(fit)

#   c) residual analysis.
#     Plot residuals vs Predicted values:
plot( fitted(fit), rstandard(fit), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(tempd, rstandard(fit), main="tempd vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

plot(tempd2, rstandard(fit), main="tempd2 vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

plot(tempd3, rstandard(fit), main="tempd3 vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(fit))
qqline(rstandard(fit), col=2)
#     Normality Test(Shapiro-Wilk Normality Test)
shapiro.test(rstandard(fit)) #If p-value>0.05, we say it follows normal distribution at 95% confidence level

#   e) Use the fitted regression model to predict the average energy consumption for an average differnece
#      in temperature equal to TEMPD = 10
#      and use the predict() function in R to produce predictions and confidence interval

# Prediction for one certain data point.
# Create new data frame containing xvalues for prediction.
new = data.frame (tempd=c(10), tempd2=c(100), tempd3=c(1000))
# Use predict() to compute predicted value and standard error
# predict(model_name, new_dataframe, ...)
# se.fit=T to compute predicted value
predict(fit, new, se.fit = T)
# compute predicted value and prediction interval
predict(fit, new, interval="prediction", level=0.95)

#   f) By using influence.measures() function to identify whether there are influential points that 
#      can affect your final model. Use cook¡¯s distance as the metric to identify the influential points
influence.measures(fit)
summary(influence.measures(fit)) # Print out only influential observations
cooks.distance(fit)
sort(cooks.distance(fit), TRUE)[1]
sort(cooks.distance(fit), TRUE)[2]
sort(cooks.distance(fit), TRUE)[3]
  