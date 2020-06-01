setwd('C:/users/mg/Desktop/Data Analytics/HW/PROJECT/project')
getwd()
mydata = read.table("Life Expectancy Data.csv", header=T, sep=",")
set.seed(1234)
newdata = mydata[c(-18)]
newdata = newdata[is.na(newdata$Life.expectancy) == FALSE, ] 
table(is.na(newdata$Alcohol))
f <- as.factor(newdata$Country) 
l <- as.list(levels(f))         
for (i in (1:214))
{
  newdata[newdata$Country==l[i],]$Alcohol = ifelse(is.na(newdata[newdata$Country==l[i],]$Alcohol),
                                                   ave(newdata[newdata$Country==l[i],]$Alcohol, FUN = function(x) mean(x, na.rm=T)), 
                                                   newdata[newdata$Country==l[i],]$Alcohol)
}
table(is.na(newdata$Alcohol))

.
newdata= newdata[-c(2377:2392),]
table(is.na(newdata$Alcohol))

l2 = c('Hepatitis.B',
       'BMI',
       'Polio',
       'Total.expenditure',
       'Diphtheria',
       'GDP',
       'thinness..1.19.years',
       'thinness.5.9.years',
       'Income.composition.of.resources',
       'Schooling',
       'infant.deaths',
       'percentage.expenditure',
       'under.five.deaths',
       'Measles')

for (i in (1:214))
  for (j in (1:14))
  {
    newdata[newdata$Country==l[i],][[l2[j]]] = ifelse(is.na(newdata[newdata$Country==l[i],][[l2[j]]]),
                                                      ave(newdata[newdata$Country==l[i],][[l2[j]]], FUN = function(x) mean(x, na.rm=T)), 
                                                      newdata[newdata$Country==l[i],][[l2[j]]])
    table(is.na(newdata[[l2[j]]])) 
  }

for (i in (1:214))
  for (j in (1:14))
  {
    newdata[newdata$Country==l[i],][[l2[j]]] = ifelse(is.na(newdata[newdata$Country==l[i],][[l2[j]]]),
                                                      ave(newdata[[l2[j]]], FUN = function(x) mean(x, na.rm=T)), 
                                                      newdata[newdata$Country==l[i],][[l2[j]]])
  }
summary(newdata)
library(countrycode)
b <- data.frame(country = newdata$Country)
newdata$Country <- countrycode(sourcevar = b[,"country"],
                               origin = "country.name",
                               destination = "continent")

library(dummies)
newdata = dummy.data.frame(newdata,names=c("Country")) 
head(newdata)

library(plyr)
newdata$Status<- revalue(newdata$Status, c("Developing"="0"))
newdata$Status<- revalue(newdata$Status, c("Developed"="1"))
newdata$Status

head(newdata)
colnames(newdata)
newdata = newdata[c(-6,-10,-14,-18,-24,-23,-12)] # linear regression을 구할 때 가진 정보를 통하여 unuseful한 x_vars를 먼저 제외시킴. 

### 지역별로 데이터 만들기 ###
oceadata = newdata[newdata$CountryOceania==1,]
oceadata = oceadata[c(-1,-2,-3,-4)]
colnames(oceadata)

#지역마다 linear regression model 만들고, variables을 비교한다.

# declation of variables(newdata)

cOc = oceadata$CountryOceania
sts = oceadata$Status
lfey = oceadata$Life.expectancy
atmy = oceadata$Adult.Mortality
achl = oceadata$Alcohol
hb = oceadata$Hepatitis.B
bmi = oceadata$BMI
ufdth = oceadata$under.five.deaths
plio = oceadata$Polio
dpria = oceadata$Diphtheria
hiv = oceadata$HIV.AIDS
gdp = oceadata$GDP
th119 = oceadata$thinness..1.19.years
sch = oceadata$Schooling

full = glm(lfey~cOc+sts+atmy+achl+hb+bmi+ufdth+plio+dpria+hiv+gdp+th119+sch) 
base = glm(lfey~achl)

# model 1  Both direction by stepwise()
m1 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=T)
summary(m1)  # AIC 760.6


# m2 Forward Selection by step()
m2 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=T) 
summary(m2) # AIC: 760.6

# m3 Backward Elimination by step() based on AIC 
m3 = step(full, direction = "backward", trace=TRUE) 
summary(m3) # AIC: 760.6

# m4 by best subset 

#install.packages("leaps")
library(leaps)
regsubsets.out <-
  regsubsets(lfey ~ cOc + sts + atmy + achl + hb + bmi + ufdth + 
               plio + dpria + hiv + gdp + th119 + sch,
             data = oceadata,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
res.legend
which.max(summary.out$adjr2)  #10
summary.out$which[10,]
m4 = glm(lfey ~ achl + hb + bmi + ufdth + 
           plio + dpria + hiv + gdp + th119 + sch)
summary(m4) # AIC: 761.64

# Conclusion: m1(=m2=m3) is best model.

#VIF test
library(car)
vif(m1) # No multicollinearity
m1 = glm(formula = lfey ~ achl + hiv+ hb + th119 + bmi + dpria)
vif(m1)

# F-test
summary(m1) #achl,gdp,hb,th119 p-value > 0.05 따라서 제거한다.
m1 = glm(formula = lfey ~ achl + hiv)
summary(m1) #F-test 완료

# Residual Analysis 

attach(mtcars)
par(mfrow=c(2,2))

#     Plot residuals vs Predicted values:
plot( fitted(m1), rstandard(m1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(achl, rstandard(m1), main="achl vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(hiv, rstandard(m1), main="hiv vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(m1))
qqline(rstandard(m1), col=2)

#conclusion : m1 is qualified.

plot(var_name, rstandard(m1), main="var_name vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

# Step 5: Model Evaluations by N-folds Cross validations
library(boot)
mse1 = cv.glm(oceadata, m1, K=10)$delta
mse1

# Removing Outliers
cooksd <- cooks.distance(m1)
influential <- as.numeric(names(cooksd)[(cooksd > (4/160))])
influential
oceadata <- oceadata[-influential, ]

# 변수 다시 선언 
cOc = oceadata$CountryOceania
sts = oceadata$Status
lfey = oceadata$Life.expectancy
atmy = oceadata$Adult.Mortality
achl = oceadata$Alcohol
hb = oceadata$Hepatitis.B
bmi = oceadata$BMI
ufdth = oceadata$under.five.deaths
plio = oceadata$Polio
dpria = oceadata$Diphtheria
hiv = oceadata$HIV.AIDS
gdp = oceadata$GDP
th119 = oceadata$thinness..1.19.years
sch = oceadata$Schooling


m1 = glm(formula = lfey ~ achl + hiv)
mse1_new = cv.glm(oceadata, m1, K=10)$delta
errs2 = cbind(mse1, mse1_new)
errs2

# mse_1 error improve 성공...
