#Life Expectancy
#Main objects : Find the factors which affect the life expectancy

# Step 0: Before starting, we changed meaningness 0 value to Null.

# Step 1: Understand the data

# Step 2: Load data, prepare the data for model fitting

setwd('C:/users/mg/Desktop/Data Analytics/HW/PROJECT/project')
getwd()
mydata = read.table("Life Expectancy Data.csv", header=T, sep=",")
set.seed(1234)
head(mydata)
str(mydata)

# declation of variables(mydata)
"""
cty = mydata$Country
yr = mydata$Year
sts = mydata$Status
lfey = mydata$Life.expectancy
atmy = mydata$Adult.Mortality
itdth = mydata$infant.deaths
achl = mydata$Alcohol
perex = mydata$percentage.expenditure
hb = mydata$Hepatitis.B
msls = mydata$Measles
bmi = mydata$BMI
ufdth = mydata$under.five.deaths
plio = mydata$Polio
totalex = mydata$Total.expenditure
dpria = mydata$Diphtheria
hiv = mydata$HIV.AIDS
gdp = mydata$GDP
pplon = mydata$Population
th119 = mydata$thinness..1.19.years
th59 = mydata$thinness.5.9.years
iccr = mydata$Income.composition.of.resources
sch = mydata$Schooling
"""


# Step 2-1: Deal with missing values
summary(mydata) # factor which has missing values: lfey, atmy, achl, hb, bmi, plio, totalex, dpria, gdp, pplon, th119, th59, iccr, sch.

#population
#population has 628 NA and unreliable value. So we delete popuation column.
newdata = mydata[c(-18)]

#life expectancy
#The countries which have no life expactancy values has not other factors. We thought this is unuseful data so delete these countries.
newdata[is.na(newdata$Life.expectancy),]
newdata = newdata[is.na(newdata$Life.expectancy) == FALSE, ]  
#Adult Motality is deleted with life expectancy

#alcohol
#Fill the missing values of alcohol with average of each countries
table(is.na(newdata$Alcohol))
f <- as.factor(newdata$Country) # switch country as factor
l <- as.list(levels(f))         # make the list which hasthese factors

for (i in (1:214))
{
  newdata[newdata$Country==l[i],]$Alcohol = ifelse(is.na(newdata[newdata$Country==l[i],]$Alcohol),
                                                   ave(newdata[newdata$Country==l[i],]$Alcohol, FUN = function(x) mean(x, na.rm=T)), 
                                                   newdata[newdata$Country==l[i],]$Alcohol)
}
table(is.na(newdata$Alcohol))

# South sudan is lack of imformation about other columns, it is not reliable so delete south sudan
newdata= newdata[-c(2377:2392),] # South Sudan 
table(is.na(newdata$Alcohol))

# Fill the other missing values like fill the missing alcohol value.
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
l2

for (i in (1:214))
  for (j in (1:14))
  {
    newdata[newdata$Country==l[i],][[l2[j]]] = ifelse(is.na(newdata[newdata$Country==l[i],][[l2[j]]]),
                                                      ave(newdata[newdata$Country==l[i],][[l2[j]]], FUN = function(x) mean(x, na.rm=T)), 
                                                      newdata[newdata$Country==l[i],][[l2[j]]])
    table(is.na(newdata[[l2[j]]])) 
    #The reason why missing values left is applicale counties don't have any information about that column. 
    #Therefore, fill these values as average of whole countries.
  }

for (i in (1:214))
  for (j in (1:14))
  {
    newdata[newdata$Country==l[i],][[l2[j]]] = ifelse(is.na(newdata[newdata$Country==l[i],][[l2[j]]]),
                                                      ave(newdata[[l2[j]]], FUN = function(x) mean(x, na.rm=T)), 
                                                      newdata[newdata$Country==l[i],][[l2[j]]])
  }
summary(newdata)


# Step 2-2: Deal with Discrete variables
#Let the discrete variables which are already discrete value.
head(newdata)


# Step 2-3: Deal with Nominal variables
# Distinguish the countries for comparing as continent.(Africa, Asia, America, Oceania, Europe)
# For the purpose of linear regression, we should convert nominal varialbes into dummy variables

#install.packages('countrycode')
library(countrycode)
b <- data.frame(country = newdata$Country)
newdata$Country <- countrycode(sourcevar = b[,"country"],
                               origin = "country.name",
                               destination = "continent")

# convert nomial(Country) variables to dummy variables.
library(dummies)
newdata = dummy.data.frame(newdata,names=c("Country")) 
head(newdata)

# Status is binary, switch as dummy variables.
# Binary 
library(plyr)
newdata$Status<- revalue(newdata$Status, c("Developing"="0"))
newdata$Status<- revalue(newdata$Status, c("Developed"="1"))
newdata$Status


# Step 2-4: Make a decision on the model evaluations
# This data set is small, and we should use N-fold cross validation in this case


# Step 3: Build Models

# declation of variables(newdata)
cAf = newdata$CountryAfrica
cAm = newdata$CountryAmericas
cAs = newdata$CountryAsia
cEu = newdata$CountryEurope
cOc= newdata$CountryOceania
yr = newdata$Year
sts = newdata$Status
lfey = newdata$Life.expectancy
atmy = newdata$Adult.Mortality
itdth = newdata$infant.deaths
achl = newdata$Alcohol
perex = newdata$percentage.expenditure
hb = newdata$Hepatitis.B
msls = newdata$Measles
bmi = newdata$BMI
ufdth = newdata$under.five.deaths
plio = newdata$Polio
totalex = newdata$Total.expenditure
dpria = newdata$Diphtheria
hiv = newdata$HIV.AIDS
gdp = newdata$GDP
th119 = newdata$thinness..1.19.years
th59 = newdata$thinness.5.9.years
iccr = newdata$Income.composition.of.resources
sch = newdata$Schooling

# Step 3-1: Examine Linear Relationship

# correlation > 0.7 : Strong linear relationship
# 0.3 < correlation < 0.7 : linear relationship
# since it is hard to interpret dummy variables and binary variables in correlation, we exclude it. ( country areas, status)
columns = cbind(yr,lfey,atmy,itdth,achl,perex,hb,msls,bmi,ufdth,plio,totalex,dpria,hiv,gdp,th119,th59,iccr,sch)
cor(columns)

#check transformed y can make correlation or not.
y = lfey
y2 = y*y
y3 = y2*y
invy = 1/y
sqrty = sqrt(y)
logy = log(y)
columns2 = cbind(y,y2,y3,invy,sqrty,logy,yr,atmy,itdth,achl,perex,hb,msls,bmi,ufdth,plio,totalex,dpria,hiv,gdp,th119,th59,iccr,sch)
cor(columns2) # <- There is no significant difference even if we do the transformation. So just use general life expectancy value as y variable.

# Comparing the low correlationship with y.
columns3 = cbind(y,yr,itdth,hb,msls,ufdth,totalex)
cor(columns3)

#comparison yr tranasformation
t1 = yr
t2 = yr*yr
t3 = log(yr)
t4 = 1/yr
t5 = sqrt(yr)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- No significant improvement, so ignore yr.

#comparison itdth tranasformation
t1 = itdth
t2 = itdth*itdth
t3 = log(itdth)
t4 = 1/itdth
t5 = sqrt(itdth)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- No significant improvement, so ignore itdth

#comparison hb tranasformation
t1 = hb
t2 = hb*hb
t3 = log(hb)
t4 = 1/hb
t5 = sqrt(hb)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- significant improvement in t2

#comparison msls tranasformation
t1 = msls
t2 = msls*msls
t3 = log(msls)
t4 = 1/msls
t5 = sqrt(msls)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- No significant improvement, so ignore msls.

#comparison ufdth tranasformation
t1 = ufdth
t2 = ufdth*ufdth
t3 = log(ufdth)
t4 = 1/ufdth
t5 = sqrt(ufdth)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- significant improvement in t3

#comparison totalex tranasformation
t1 = totalex
t2 = totalex*totalex
t3 = log(totalex)
t4 = 1/totalex
t5 = sqrt(totalex)
tcol = cbind(y,t1,t2,t3,t4,t5)
cor(tcol) # <- No significant improvement, so ignore totalex

###Conclusion: (ignore yr,msls,totalex), (transform sqrt(itdth), hb2, log(ufdth))
newdata = newdata[c(-6,-10,-14,-18)] #(ignore yr,itdth,msls,totalex)

#add transformed new variables
hb2 = hb*hb
newdata[,"hb2"]=hb2
logufdth = log(ufdth)
newdata[,"logufdth"]=logufdth



# Step 3-2: Build models by feature selection


################# Linear regression model #############
#By doing VIF test for the first model, sove the multicollinearity problem between x variables.
library(car)
#build basic model(Just only one x variables)
basicfull = glm(lfey~cAf + cAm+ cAs+cOc+
                  sts+ 
                  achl+perex+bmi+gdp+th119+th59+iccr+sch+hiv+ 
                  atmy+logufdth+hb2+plio+dpria)

# Evaluate MultiCollnearity
vif(basicfull) #iccr>th59>th119>sch>gdp>perex>cAf>

basicfull = glm(lfey~ cAm+ cAs+cOc+ #iccr,th59,perex,cAf,cEu are removed.
                  sts+ 
                  achl+bmi+gdp+th119+sch+hiv+ 
                  atmy+logufdth+hb2+plio+dpria)
vif(basicfull)

############  Solved multicolinearity problem   ############ 

#build full model CASE1
full_1 = glm(lfey~ cAm+ cAs+cOc+ #iccr,th59,perex,cAf,cEu are removed.
               sts+ 
               achl+bmi+gdp+th119+sch+hiv+ 
               atmy+logufdth+hb2+plio+dpria)

#build base model CASE1
base_1 = glm(lfey~achl,data=newdata)

# model 1_1  Both direction by stepwise()
m1_1 = step(base_1, scope=list(upper=full_1, lower=~1), direction="both", trace=T)
summary(m1_1)  # aic 15990

# model 1_2 Forward Selection by step()
m1_2 = step(base_1, scope=list(upper=full_1, lower=~1), direction="forward", trace=T) 
summary(m1_2) # AIC: 16000

# model 1_3 Backward Elimination by step() based on AIC 
m1_3 = step(full_1, direction = "backward", trace=TRUE) 
summary(m1_3) # AIC: 15990

# model 1_4 by best subset 

#install.packages("leaps")
library(leaps)
regsubsets.out <-
  regsubsets(lfey~cAm+cAs+cOc+
               sts+
               achl+bmi+gdp+th119+sch+hiv+
               atmy+logufdth+hb2+plio+dpria,
             data = newdata,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
res.legend
which.max(summary.out$adjr2)  #13
summary.out$which[13,]
m1_4 = glm(formula = lfey~cAm+cAs+cOc+
             sts+
             achl+bmi+gdp+th119+sch+hiv+
             atmy+logufdth+hb2+plio+dpria,data=newdata)
summary(m1_4) # AIC - 16000


#model m1_1 and  m1_3 are same
#Conclusion: glm(formula = lfey ~ cAm + cAs + cOc + sts + bmi + gdp + th119 + sch + hiv + atmy + logufdth + plio + dpria) is the best model.


############ Polynomial Model // CASE_2 ##################

#comparing the plots for checking the polynomial or not
attach(mtcars)
par(mfrow=c(4,3))
plot(achl,y)
plot(perex,y)
plot(bmi,y)
plot(gdp,y)
plot(th119,y)
plot(th59,y)
plot(iccr,y)
plot(sch,y)
plot(hiv,y)
plot(atmy,y)
plot(logufdth,y)
plot(hb2,y)

#Conclusion: th119,th59 and logufdth are suspected as polynomial. th59 is deleted when multicollinearity problem is solved.

###  polynominal check  ###
# add th119^2, th119^3, logufdth^2 and logufdth^3 to x-var.
th1192 = th119^2
th1193 = th119^3
th592 = th59^2
th593 = th59^3
logufdth2 = logufdth^2
logufdth3 = logufdth^3

full_2 = glm(lfey~cAm+cAs+cOc+
               sts+
               achl+bmi+gdp+th119+th1192+th1193+
               sch+hiv+atmy+logufdth+logufdth2+logufdth3+hb2+plio+dpria,
             data=newdata)
vif(full_2)

full_2 = glm(lfey~cAm+cAs+cOc+
               sts+
               achl+bmi+gdp+th1193+
               sch+hiv+atmy+logufdth+hb2+plio+dpria,
             data=newdata)
mvif(full_2) #Solve multicollinearity

#build base model CASE1
base_2 = glm(lfey~achl,data=newdata)

# model 2_1  Both direction by stepwise()
m2_1 = step(base_2, scope=list(upper=full_2, lower=~1), direction="both", trace=T)
summary(m2_1)  # aic 16010

# model 2_2 Forward Selection by step()
m2_2 = step(base_2, scope=list(upper=full_2, lower=~1), direction="forward", trace=T) 
summary(m2_2) # AIC: 16010

# model 2_3 Backward Elimination by step() based on AIC 
m2_3 = step(full_2, direction = "backward", trace=TRUE) 
summary(m2_3) # AIC: 16010

# model 2_4 by best subset 
#install.packages("leaps")
library(leaps)
regsubsets.out <-
  regsubsets(lfey~cAm+cAs+cOc+
               sts+
               achl+bmi+gdp+th1193+
               sch+hiv+atmy+logufdth+hb2+plio+dpria,
             data = newdata,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
res.legend
which.max(summary.out$adjr2)  #12
summary.out$which[12,]
m2_4 = glm(formula = lfey~cAm+cAs+cOc+
             sts+
             bmi+gdp+sch+hiv+
             atmy+logufdth+plio+dpria,data=newdata)
summary(m2_4) # AIC - 16010

# Conclusion: No better model then case_1.
# All models are same so we do as m2_1

############ Polynomial Model // CASE_3 ##################

############ INTERACTION TERM check ################
#install.packages('sjPlot')
#install.packages('sjmisc')
#install.packages('ggplot2')
#install.packages('snakecase')
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(snakecase)

theme_set(theme_sjplot())

# fit model with interaction

tf1 <- glm(y ~ achl * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("achl", "sts"))
tf1 <- glm(y ~ perex * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("perex", "sts"))
tf1 <- glm(y ~ bmi * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("bmi", "sts"))
tf1 <- glm(y ~ gdp * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("gdp", "sts"))
tf1 <- glm(y ~ th119 * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("th119", "sts"))
tf1 <- glm(y ~ th59 * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("th59", "sts"))
tf1 <- glm(y ~ iccr * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("iccr", "sts"))
tf1 <- glm(y ~ sch * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("sch", "sts"))
tf1 <- glm(y ~ hiv * sts, data = newdata) 
plot_model(tf1, type = "int", terms = c("hiv", "sts")) #Not Interaction term
tf1 <- glm(y ~ atmy * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("atmy", "sts"))
tf1 <- glm(y ~ hb2 * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("hb2", "sts"))
tf1 <- glm(y ~ logufdth * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("logufdth", "sts"))
tf1 <- glm(y ~ plio * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("plio", "sts"))
tf1 <- glm(y ~ dpria * sts, data = newdata)
plot_model(tf1, type = "int", terms = c("dpria", "sts"))
# (achl*sts)+(perex*sts)+(bmi*sts)+(gdp*sts)+(th119*sts)+(th59*sts)+(iccr*sts)+(sch*sts)+(atmy*sts)+(hb2*sts)+(logufdth*sts)+(plio*sts)+(dpria*sts)


tf2 <- glm(y ~ achl * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("achl", "cAf"))
tf2 <- glm(y ~ perex * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("perex", "cAf"))
tf2 <- glm(y ~ bmi * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("bmi", "cAf"))
tf2 <- glm(y ~ gdp * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("gdp", "cAf"))
tf2 <- glm(y ~ th119 * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("th119", "cAf")) # Not
tf2 <- glm(y ~ th59 * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("th59", "cAf")) # Not
tf2 <- glm(y ~ iccr * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("iccr", "cAf")) # Not
tf2 <- glm(y ~ sch * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("sch", "cAf"))
tf2 <- glm(y ~ hiv * cAf, data = newdata) 
plot_model(tf2, type = "int", terms = c("hiv", "cAf"))
tf2 <- glm(y ~ atmy * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("atmy", "cAf"))
tf2 <- glm(y ~ hb2 * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("hb2", "cAf"))
tf2 <- glm(y ~ logufdth * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("logufdth", "cAf"))
tf2 <- glm(y ~ plio * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("plio", "cAf")) # Not
tf2 <- glm(y ~ dpria * cAf, data = newdata)
plot_model(tf2, type = "int", terms = c("dpria", "cAf")) # Not
# (achl*cAf)+(perex*cAf)+(bmi*cAf)+(gdp*cAf)+(sch*cAf)+(hiv*cAf)+(atmy*cAf)+(hb2*cAf)+(logufdth*cAf)



tf3f <- glm(y ~ achl * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("achl", "cAm"))
tf3f <- glm(y ~ perex * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("perex", "cAm"))
tf3f <- glm(y ~ bmi * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("bmi", "cAm"))
tf3f <- glm(y ~ gdp * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("gdp", "cAm"))
tf3f <- glm(y ~ th119 * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("th119", "cAm"))
tf3f <- glm(y ~ th59 * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("th59", "cAm"))
tf3f <- glm(y ~ iccr * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("iccr", "cAm"))
tf3f <- glm(y ~ sch * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("sch", "cAm"))
tf3f <- glm(y ~ hiv * cAm, data = newdata) 
plot_model(tf3f, type = "int", terms = c("hiv", "cAm"))
tf3f <- glm(y ~ atmy * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("atmy", "cAm"))
tf3f <- glm(y ~ hb2 * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("hb2", "cAm"))
tf3f <- glm(y ~ logufdth * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("logufdth", "cAm"))
tf3f <- glm(y ~ plio * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("plio", "cAm"))
tf3f <- glm(y ~ dpria * cAm, data = newdata)
plot_model(tf3f, type = "int", terms = c("dpria", "cAm"))
# (achl*cAm)+(perex*cAm)+(bmi*cAm)+(gdp*cAm)+(th119*cAm)+(th59*cAm)+(iccr*cAm)+(sch*cAm)+(hiv*cAm)+(atmy*cAm)+(hb2*cAm)+(logufdth*cAm)+(plio*cAm)+(dpria*cAm)


tf4 <- glm(y ~ achl * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("achl", "cAs"))
tf4 <- glm(y ~ perex * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("perex", "cAs"))
tf4 <- glm(y ~ bmi * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("bmi", "cAs"))
tf4 <- glm(y ~ gdp * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("gdp", "cAs"))
tf4 <- glm(y ~ th119 * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("th119", "cAs"))
tf4 <- glm(y ~ th59 * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("th59", "cAs"))
tf4 <- glm(y ~ iccr * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("iccr", "cAs"))
tf4 <- glm(y ~ sch * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("sch", "cAs"))
tf4 <- glm(y ~ hiv * cAs, data = newdata) 
plot_model(tf4, type = "int", terms = c("hiv", "cAs"))
tf4 <- glm(y ~ atmy * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("atmy", "cAs"))
tf4 <- glm(y ~ hb2 * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("hb2", "cAs")) # Not
tf4 <- glm(y ~ logufdth * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("logufdth", "cAs"))
tf4 <- glm(y ~ plio * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("plio", "cAs"))
tf4 <- glm(y ~ dpria * cAs, data = newdata)
plot_model(tf4, type = "int", terms = c("dpria", "cAs"))
# (achl*cAs)+(perex*cAs)+(bmi*cAs)+(gdp*cAs)+(th119*cAs)+(th59*cAs)+(iccr*cAs)+(sch*cAs)+(hiv*cAs)+(atmy*cAs)+(logufdth*cAs)+(plio*cAs)+(dpria*cAs)


tf5 <- glm(y ~ achl * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("achl", "cEu"))
tf5 <- glm(y ~ perex * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("perex", "cEu"))
tf5 <- glm(y ~ bmi * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("bmi", "cEu"))
tf5 <- glm(y ~ gdp * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("gdp", "cEu"))
tf5 <- glm(y ~ th119 * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("th119", "cEu"))
tf5 <- glm(y ~ th59 * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("th59", "cEu"))
tf5 <- glm(y ~ iccr * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("iccr", "cEu"))
tf5 <- glm(y ~ sch * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("sch", "cEu"))
tf5 <- glm(y ~ hiv * cEu, data = newdata) 
plot_model(tf5, type = "int", terms = c("hiv", "cEu"))
tf5 <- glm(y ~ atmy * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("atmy", "cEu")) #Not
tf5 <- glm(y ~ hb2 * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("hb2", "cEu"))
tf5 <- glm(y ~ logufdth * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("logufdth", "cEu"))
tf5 <- glm(y ~ plio * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("plio", "cEu"))
tf5 <- glm(y ~ dpria * cEu, data = newdata)
plot_model(tf5, type = "int", terms = c("dpria", "cEu"))
# (achl*cEu)+(perex*cEu)+(bmi*cEu)+(gdp*cEu)+(th119*cEu)+(th59*cEu)+(iccr*cEu)+(sch*cEu)+(hiv*cEu)+(hb2*cEu)+(logufdth*cEu)+(plio*cEu)+(dpria*cEu)


tf6 <- glm(y ~ achl * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("achl", "cOc"))
tf6 <- glm(y ~ perex * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("perex", "cOc"))
tf6 <- glm(y ~ bmi * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("bmi", "cOc"))
tf6 <- glm(y ~ gdp * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("gdp", "cOc"))
tf6 <- glm(y ~ th119 * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("th119", "cOc"))
tf6 <- glm(y ~ th59 * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("th59", "cOc"))
tf6 <- glm(y ~ iccr * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("iccr", "cOc"))
tf6 <- glm(y ~ sch * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("sch", "cOc"))
tf6 <- glm(y ~ hiv * cOc, data = newdata) 
plot_model(tf6, type = "int", terms = c("hiv", "cOc"))
tf6 <- glm(y ~ atmy * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("atmy", "cOc"))
tf6 <- glm(y ~ hb2 * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("hb2", "cOc"))
tf6 <- glm(y ~ logufdth * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("logufdth", "cOc"))
tf6 <- glm(y ~ plio * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("plio", "cOc"))
tf6 <- glm(y ~ dpria * cOc, data = newdata)
plot_model(tf6, type = "int", terms = c("dpria", "cOc"))
# (achl*cOc)+(perex*cOc)+(bmi*cOc)+(gdp*cOc)+(th119*cOc)+(th59*cOc)+(iccr*cOc)+(sch*cOc)+(hiv*cOc)+(atmy*cOc)+(hb2*cOc)+(logufdth*cOc)+(plio*cOc)+(dpria*cOc)

# Interaction Terms: (Exclude the multicollinearity problemed x-variables.)
# (achl*sts)+(bmi*sts)+(gdp*sts)+(th119*sts)+(sch*sts)+(atmy*sts)+(hb2*sts)+(logufdth*sts)+(plio*sts)+(dpria*sts)
# (achl*cAm)+(bmi*cAm)+(gdp*cAm)+(th119*cAm)+(sch*cAm)+(hiv*cAm)+(atmy*cAm)+(hb2*cAm)+(logufdth*cAm)+(plio*cAm)+(dpria*cAm)
# (achl*cAs)+(bmi*cAs)+(gdp*cAs)+(th119*cAs)+(sch*cAs)+(hiv*cAs)+(atmy*cAs)+(logufdth*cAs)+(plio*cAs)+(dpria*cAs)
# (achl*cOc)+(bmi*cOc)+(gdp*cOc)+(th119*cOc)+(sch*cOc)+(hiv*cOc)+(atmy*cOc)+(hb2*cOc)+(logufdth*cOc)+(plio*cOc)+(dpria*cOc)

full_3 = glm(lfey~ cAm+ cAs+cOc+ 
               sts+ 
               achl+bmi+gdp+th119+sch+hiv+ 
               atmy+logufdth+hb2+plio+dpria+
               (achl*sts)+(bmi*sts)+(gdp*sts)+(th119*sts)+(sch*sts)+(atmy*sts)+(hb2*sts)+(logufdth*sts)+(plio*sts)+(dpria*sts)+
               (achl*cAm)+(bmi*cAm)+(gdp*cAm)+(th119*cAm)+(sch*cAm)+(hiv*cAm)+(atmy*cAm)+(hb2*cAm)+(logufdth*cAm)+(plio*cAm)+(dpria*cAm)+
               (achl*cAs)+(bmi*cAs)+(gdp*cAs)+(th119*cAs)+(sch*cAs)+(hiv*cAs)+(atmy*cAs)+(logufdth*cAs)+(plio*cAs)+(dpria*cAs)+
               (achl*cOc)+(bmi*cOc)+(gdp*cOc)+(th119*cOc)+(sch*cOc)+(hiv*cOc)+(atmy*cOc)+(hb2*cOc)+(logufdth*cOc)+(plio*cOc)+(dpria*cOc))

full_3 = glm(lfey~
               achl+bmi+gdp+th119+sch+hiv+ 
               atmy+logufdth+hb2+plio+dpria+
               (gdp*cAm)+(hiv*cAm)+
               (achl*cAs)+(gdp*cAs)+(hiv*cAs)+
               (gdp*cOc)+(th119*cOc)+(hiv*cOc))
vif(full_3)
max(vif(full_3)) # all the multicollinearity variables are removed.


#build base model CASE1
base_3 = glm(lfey~achl,data=newdata)

# model 3_1  Both direction by stepwise()
m3_1 = step(base_3, scope=list(upper=full_3, lower=~1), direction="both", trace=T)
summary(m3_1)  # aic 16124

# model 3_2 Forward Selection by step()
m3_2 = step(base_3, scope=list(upper=full_3, lower=~1), direction="forward", trace=T) 
summary(m3_2) # AIC: 16124

# model 3_3 Backward Elimination by step() based on AIC 
m3_3 = step(full_3, direction = "backward", trace=TRUE) 
summary(m3_3) # AIC: 16124

# model 3_4 by best subset 
#install.packages("leaps")
library(leaps)
regsubsets.out <-
  regsubsets(lfey~
               achl+bmi+gdp+th119+sch+hiv+ 
               atmy+logufdth+hb2+plio+dpria+
               (gdp*cAm)+(hiv*cAm)+
               (achl*cAs)+(gdp*cAs)+(hiv*cAs)+
               (gdp*cOc)+(th119*cOc)+(hiv*cOc),
             data = newdata,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
res.legend
which.max(summary.out$adjr2)  #12
summary.out$which[19,]
m3_4 = glm(lfey~
             achl+bmi+gdp+th119+sch+hiv+ 
             atmy+logufdth+plio+dpria+
             (gdp*cAm)+(hiv*cAm)+
             (achl*cAs)+(gdp*cAs)+
             (gdp*cOc)+(th119*cOc),data=newdata)
summary(m3_4) # AIC - 16124

# Conclusion: No better model then case_1.
# All models are same so we do as m3_1.

# AIC is smaller in order of m1_1 > m2_1 > m3_1

# Step 4: Examine the model is qualifeid
# We don't need to do F-test because adopt the N-fold cross validatiaon.

####### Residual Analysis of m1_1 #######
attach(mtcars)
par(mfrow=c(4,4))


#     Plot residuals vs Predicted values:
plot( fitted(m1_1), rstandard(m1_1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(sch, rstandard(m1_1), main="sch vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(atmy, rstandard(m1_1), main="atmy vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(hiv, rstandard(m1_1), main="hiv vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(dpria, rstandard(m1_1), main="dpria vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(bmi, rstandard(m1_1), main="bmi vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAs, rstandard(m1_1), main="cAs vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAm, rstandard(m1_1), main="cAm vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(sts, rstandard(m1_1), main="sts vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(gdp, rstandard(m1_1), main="gdp vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(logufdth, rstandard(m1_1), main="logufdth vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(plio, rstandard(m1_1), main="plio vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(th119, rstandard(m1_1), main="th119 vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cOc, rstandard(m1_1), main="cOc vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

# Draw normal probability plot of residuals:
qqnorm(rstandard(m1_1))
qqline(rstandard(m1_1), col=2)

# Conclusion : m1_1 is qualified.

####### Residual Analysis of m2_1 #######
attach(mtcars)
par(mfrow=c(4,4))
#     Plot residuals vs Predicted values:
plot( fitted(m2_1), rstandard(m2_1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(sch, rstandard(m2_1), main="sch vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(atmy, rstandard(m2_1), main="atmy vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(hiv, rstandard(m2_1), main="hiv vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(dpria, rstandard(m2_1), main="dpria vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(bmi, rstandard(m2_1), main="bmi vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAs, rstandard(m2_1), main="cAs vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAm, rstandard(m2_1), main="cAm vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(sts, rstandard(m2_1), main="sts vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(gdp, rstandard(m2_1), main="gdp vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(logufdth, rstandard(m2_1), main="logufdth vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(plio, rstandard(m2_1), main="plio vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cOc, rstandard(m2_1), main="cOc vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Draw normal probability plot of residuals:
qqnorm(rstandard(m2_1))
qqline(rstandard(m2_1), col=2)

#conclusion : m2_1 is qualified.


####### Residual Analysis of m3_1 #######
attach(mtcars)
par(mfrow=c(4,4))
#     Plot residuals vs Predicted values:
plot( fitted(m3_1), rstandard(m3_1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

#     Plot residuals vs each x-variable:
plot(achl, rstandard(m3_1), main="achl vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(sch, rstandard(m3_1), main="sch vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(atmy, rstandard(m3_1), main="atmy vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(hiv, rstandard(m3_1), main="hiv vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(dpria, rstandard(m3_1), main="dpria vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(bmi, rstandard(m3_1), main="bmi vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAs, rstandard(m3_1), main="cAs vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cAm, rstandard(m3_1), main="cAm vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(gdp, rstandard(m3_1), main="gdp vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(logufdth, rstandard(m3_1), main="logufdth vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(plio, rstandard(m3_1), main="plio vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(th119, rstandard(m3_1), main="th119 vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(cOc, rstandard(m3_1), main="cOc vs residuals plot")
abline(a=0, b=0, col='red') # add zero line

inter1 = hiv*cAm
inter2 = cAm*gdp
inter3 = cAs*gdp
inter4 = gdp*cOc
inter5 = th119*cOc

plot(inter1, rstandard(m3_1), main="hiv:cAm vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(inter2, rstandard(m3_1), main="cAm:gdp vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(inter3, rstandard(m3_1), main="cAs:gdp vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(inter4, rstandard(m3_1), main="gdp:cOc vs residuals plot")
abline(a=0, b=0, col='red') # add zero line
plot(inter5, rstandard(m3_1), main="th119:cOc vs residuals plot")
abline(a=0, b=0, col='red') # add zero line


# Draw normal probability plot of residuals:
qqnorm(rstandard(m3_1))
qqline(rstandard(m3_1), col=2)

#conclusion : m3_1 is qualified.

# Step 5: Model Evaluations by N-folds Cross validations
library(boot)
m1_1 = glm(lfey ~ sch + atmy + hiv + dpria + bmi + cAs + cAm + 
             sts + gdp + log(newdata$under.five.deaths) + plio + th119 + cOc, data = newdata) 

m2_1 = glm(lfey ~ sch + atmy + hiv + dpria + bmi + cAs + cAm + 
             sts + gdp + log(newdata$under.five.deaths) + plio + cOc, data = newdata)

m3_1 = glm(lfey ~ achl + sch + atmy + hiv + dpria + bmi + 
             cAs + cAm + gdp + log(newdata$under.five.deaths) + plio + th119 + cOc + hiv:cAm + 
             cAm:gdp + cAs:gdp + gdp:cOc + th119:cOc, data = newdata)

mse1 = cv.glm(newdata, m1_1, K=10)$delta
mse2 = cv.glm(newdata, m2_1, K=10)$delta
mse3 = cv.glm(newdata, m3_1, K=10)$delta

errs = cbind(mse1, mse2, mse3)
errs
#conclusion: m1_1 has less error then others so we chose m1_1 as a best model.

# To improve the model, remove influential points
# The criteria is 4/n = 4/2888 = 0.001385042. If cook.d > 0.001385042, it is influential points.

# Removing Outliers
# influential row numbers
#install.packages('tidyverse')
#install.packages('gridExtra')
library(tidyverse)
library(gridExtra)
cooksd <- cooks.distance(m1_1)
influential <- as.numeric(names(cooksd)[(cooksd > (4/2888))])
influential

# Option) Alternatively, we can try to remove the top x outliers to have a look
# influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))
newdata2 <- newdata[-influential, ]
cAm = newdata2$CountryAmericas
cAs = newdata2$CountryAsia
cOc= newdata2$CountryOceania
sts = newdata2$Status
lfey = newdata2$Life.expectancy
atmy = newdata2$Adult.Mortality
bmi = newdata2$BMI
ufdth = newdata2$under.five.deaths
plio = newdata2$Polio
dpria = newdata2$Diphtheria
hiv = newdata2$HIV.AIDS
gdp = newdata2$GDP
th119 = newdata2$thinness..1.19.years
sch = newdata2$Schooling

m1_1_new = glm(lfey ~ sch + atmy + hiv + dpria + bmi + cAs + cAm + 
                 sts + gdp + log(newdata2$under.five.deaths) + plio + th119 + cOc, data = newdata2)
mse1_new = cv.glm(newdata2, m1_1_new, K=10)$delta
errs2 = cbind(mse1, mse1_new)
errs2

# mse_1 error is improved
m1_1_new
summary(m1_1_new)

# Conclusion: m1_1_new is the best model.
# Variables which are belonged in m1_1_new model affect factors to life expectancy.
# m1_1_new = glm(lfey ~ sch + atmy + hiv + dpria + bmi + cAs + cAm + sts + gdp + log(newdata2$under.five.deaths) + plio + th119 + cOc, data = newdata2)


# This model calculated by worldwide data.