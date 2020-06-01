# Step 0: 시작전에 엑셀파일을 살펴보았을 때 NULL이 아닌 0으로된 값들이 너무 많아서 전부 NULL로 바꾸었다.

# Step 1: Understand the data
# Word file 참고





# Step 2: Load data, prepare the data for model fitting

setwd('C:/users/mg/Desktop/Data Analytics/HW/PROJECT/project')
getwd()
mydata = read.table("Life Expectancy Data.csv", header=T, sep=",")
set.seed(1234)
head(mydata)
str(mydata)

# Step 2-1: Deal with missing values
summary(mydata) # factor which has missing values: lfey, atmy, achl, hb, bmi, plio, totalex, dpria, gdp, pplon, th119, th59, iccr, sch.

#pplon
#첫 번째 필터링
#population의 경우 628개의 NA가 있는데, 이미 존재하는 value들을 보더라도 하나의 국가에서 연간 격차가 매우 크다. 이는 불가능하므로 population column 자체를 삭제한다.
newdata = mydata[c(-18)]

#lfey
#lfey 가 없는 국가들은 모두 하나의 인덱스 밖에 없는 국가들로, 해당 국가의 다른 정보 factors도 비어있기에 우리의 목표에 unuseful 하다고 판단하여 삭제한다.
newdata[is.na(newdata$Life.expectancy),] #lfey가 NA인 국가들 보기
# 두번째 필터링, life.expectancy가 na인 row를 제외하고 만든다.
newdata = newdata[is.na(newdata$Life.expectancy) == FALSE, ]  
#atmy : lfey와 같이 없어짐.

#achl
#214개 국가에 대하여 각 국가의 missing values of alcohol을 해당 국가의 평균 값으로 채운다.
# ifelse(조건,조건이 TRUE일 경우 ,FALSE일 경우)
#newdata[newdata$Country=='Angola',]$Alcohol # Country == Angola 중에 alcohol column 부분
#is.na(newdata[newdata$Country=='Angola',]$Alcohol) # 조건:Country == Angola 이고 newdata의 Alcohol Column에서 NA value를 가지는 부분
#ave(newdata[newdata$Country=='Angola',]$Alcohol, FUN = function(x) mean(x, na.rm=T)) # country == Angola인 alcohol column의 평균값으로 모두 바꾼다.
table(is.na(newdata$Alcohol))

# for문을 이용하기위해
f <- as.factor(newdata$Country) # newdata$Country를 factor로 바꾼다.
l <- as.list(levels(f))         # 바꾼 country factor를 list로 바꾼다.

for (i in (1:214))
{
newdata[newdata$Country==l[i],]$Alcohol = ifelse(is.na(newdata[newdata$Country==l[i],]$Alcohol),
                                                     ave(newdata[newdata$Country==l[i],]$Alcohol, FUN = function(x) mean(x, na.rm=T)), 
                                                     newdata[newdata$Country==l[i],]$Alcohol)
}
table(is.na(newdata$Alcohol))

# 위의 for문을 돌려보니 South Sudan의 경우에는 Alcohol을 포함한 많은 columns에서 정보가 부족해 우리의 목표에 부적합 한 것 같아 south sudan 자체를 삭제하였다. 
newdata= newdata[-c(2377:2392),] # South Sudan 삭제
table(is.na(newdata$Alcohol))

#이 아래부터는 acohol과 방법이 동일하다.
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

# data$column_name is equal to data[['column_name']] // different from data['column_name']

for (i in (1:214))
  for (j in (1:14))
  {
    newdata[newdata$Country==l[i],][[l2[j]]] = ifelse(is.na(newdata[newdata$Country==l[i],][[l2[j]]]),
                                               ave(newdata[newdata$Country==l[i],][[l2[j]]], FUN = function(x) mean(x, na.rm=T)), 
                                               newdata[newdata$Country==l[i],][[l2[j]]])
    table(is.na(newdata[[l2[j]]])) #Missing value가 남았는데, 남은 이유는 어떤 국가에 해당 column에 대한 정보가 전혀 없기 때문이다.
    #따라서 전체 국가의 평균값을 넣는다.
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
head(newdata)
#year를 포함 모든 discrete varialbe들은 내버려 둔다.

#year
#adult mortality
#infant deaths
#hepatitis B
#measles
#under-five deaths
#total expenditure
#diphtheria

# Step 2-3: Deal with Nominal variables

#시작을 하기 전에 우리는 대륙별로 비교하는 것으로 간소화 하고자 하기에, 국가들을 나누었다.
# 아프리카, 유럽, 중동아시아, 동아시아, 오세아니아, 아메리카
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

# Status는 binary이고, be considered as discrete
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

# y=lfey를 transformation해서 상관관계가 증가되는지 확인
y = lfey
y2 = y*y
y3 = y2*y
invy = 1/y
sqrty = sqrt(y)
logy = log(y)
columns2 = cbind(y,y2,y3,invy,sqrty,logy,yr,atmy,itdth,achl,perex,hb,msls,bmi,ufdth,plio,totalex,dpria,hiv,gdp,th119,th59,iccr,sch)
cor(columns2) # <- transformation을 하더라도 significant한 변화가 보이지 않기에 transformation을 하지 않는다.

# y와 상관관계가 적은 애들만 다시 비교
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
#첫 모델에 대한 VIF를 진행해서 x variable간 multicollinearity problem을 해결한다.
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

############  Solved multicolinearity problem   ############ 모델마다해야함

#build full model CASE1
full_1 = glm(lfey~ cAm+ cAs+cOc+ #iccr,th59,perex,cAf,cEu are removed.
                  sts+ 
                  achl+bmi+gdp+th119+sch+hiv+ 
                  atmy+logufdth+hb2+plio+dpria)

#build base model CASE1
base_1 = glm(lfey~achl,data=newdata)

# model 1  Both direction by stepwise()
m1_1 = step(base_1, scope=list(upper=full_1, lower=~1), direction="both", trace=T)
summary(m1_1)  # aic 15990

# m2 Forward Selection by step()
m1_2 = step(base_1, scope=list(upper=full_1, lower=~1), direction="forward", trace=T) 
summary(m1_2) # AIC: 16000

# m3 Backward Elimination by step() based on AIC 
m1_3 = step(full_1, direction = "backward", trace=TRUE) 
summary(m1_3) # AIC: 15990

# m1_4 by best subset 

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

#Conclusion: 
#m1_1, m1_3 are the best models 
# glm(formula = lfey ~ cAm + cAs + cOc + sts + bmi + gdp + th119 + sch + hiv + atmy + logufdth + plio + dpria)






############ Polynomial Model // CASE_2 ##################

#plot비교 Polynomial인지 아닌지 확인하기위해.
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
#Conclusion: th119,th59, logufdth가 polynomial로 의심된다. th59는 multicollinearity problem에서 삭제됨.

###  polynominal  -  x-var에 th119^2, th119^3, logufdth^2, logufdth^3 추가 -  
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
vif(full_2) #Multicollinearity 해결

#build base model CASE1
base_2 = glm(lfey~achl,data=newdata)

# model 1  Both direction by stepwise()
m2_1 = step(base_2, scope=list(upper=full_2, lower=~1), direction="both", trace=T)
summary(m2_1)  # aic 16010

# m2 Forward Selection by step()
m2_2 = step(base_2, scope=list(upper=full_2, lower=~1), direction="forward", trace=T) 
summary(m2_2) # AIC: 16010

# m3 Backward Elimination by step() based on AIC 
m2_3 = step(full_2, direction = "backward", trace=TRUE) 
summary(m2_3) # AIC: 16010

# m2_4 by best subset 
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

# Conclusion: There is no model which in better than case_1 when compared models by using AIC
# Anyway, the best model is m2_1 in here.

############ Polynomial Model // CASE_3 ##################

########INTERACTION TERM이 무엇인가를 알기위해########
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

#따라서 총 Interaction Terms: (multicollinearity problem이 있는 x var는 포함시키지 않음.)
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

# model 1  Both direction by stepwise()
m3_1 = step(base_3, scope=list(upper=full_3, lower=~1), direction="both", trace=T)
summary(m3_1)  # aic 16124

# m2 Forward Selection by step()
m3_2 = step(base_3, scope=list(upper=full_3, lower=~1), direction="forward", trace=T) 
summary(m3_2) # AIC: 16124

# m3 Backward Elimination by step() based on AIC 
m3_3 = step(full_3, direction = "backward", trace=TRUE) 
summary(m3_3) # AIC: 16124

# m3_4 by best subset 
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

# COnclusion: There is no model better than CASE_1
# Anyway, best model is m3_1 in here.

# 따라서 m1_1 > m2_1 > m3_1 순으로 AIC가 작음. 

# Step 4: Examine the model is qualifeid

# F-test
summary(m1_1)
summary(m2_1)
summary(m3_1) #모든 모델이 통과함.

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

#     Draw normal probability plot of residuals:
qqnorm(rstandard(m1_1))
qqline(rstandard(m1_1), col=2)

#conclusion : m1_1 is qualified.





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


#     Draw normal probability plot of residuals:
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
#conclusion: So the best model is m1_1

# To improve the model,
# Influential Points
#The criteria is 4/n = 4/2888 = 0.001385042. If cook.d > 0.001385042, it is influential points.

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
             sts + gdp + log(newdata2$under.five.deaths) + plio + th119 + cOc, data = newdata2) #에러나서 재설정
mse1_new = cv.glm(newdata2, m1_1_new, K=10)$delta
errs2 = cbind(mse1, mse1_new)
errs2

# mse_1 error improve 성공...

m1_1_new
summary(m1_1_new)

# 이 모델은 전세계 모델 기준.
  