# Step 0: 시작전에 엑셀파일을 살펴보았을 때 NULL이 아닌 0으로된 값들이 너무 많아서 전부 NULL로 바꾸었다.

# Step 1: Understand the data
# Word file 참고





# Step 2: Load data, prepare the data for model fitting

getwd()
setwd('D:/Data Analytics/Final project/')
mydata=read.table('Life Expectancy Data.csv',header=T,',')
set.seed(1234)
head(mydata)
str(mydata)

# declation of variables(mydata)
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

###x-variables transformation

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

###Conclusion: (ignore variables: yr,msls,totalex), (transform sqrt(itdth), hb2, ㅣㅐㅎ(ufdth))
newdata = newdata[c(-6,-10,-14,-18)] #(ignore yr,itdth,msls,totalex)

#add transformed new variables
hb2 = hb*hb
newdata[,"hb2"]=hb2
logufdth = log(ufdth)
newdata[,"logufdth"]=logufdth

# Step 3-2: Build models by feature selection
#x-variables: Africa, America, Asia, Europe, Oceania, Status, Alcohol,Percentage.expenditure,bmi,thinness.1.19.years,thinness.5.9.years, 
#Income.composition.of.resources, schooling, hiv, adult.mortality, log(under.five.deaths), Hepatitis.B*2

#poly nominal 
#build full model
#install.packages('car')
library(car)
base = glm(lfey~cAf,data=newdata)
full = glm(lfey~cAf+cAm+cAs+cEu+cOc+
             sts+
             achl+perex+bmi+gdp+th119+th59+iccr+sch+hiv+
             atmy+logufdth+hb2,data=newdata)
# model 1  Both direction by stepwise()
m1 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=T)
summary(m1)  # aic 15575

# m2 Forward Selection by step()
m2 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=T) 
summary(m3) # AIC: 15575

# m3 Backward Elimination by step() based on AIC 
m3= step(full, direction = "backward", trace=TRUE) 
summary(m3) # AIC:15575

# m4 by best subset 
#install.packages("leaps")
library(leaps)
regsubsets.out <-
  regsubsets(lfey~cAf+cAm+cAs+cEu+cOc+
               sts+
               achl+perex+bmi+gdp+th119+th59+iccr+sch+hiv+
               atmy+logufdth+hb2,
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
m4 = glm(lfey~cAf+cAm+cEu+
           sts+
           achl+perex+bmi+gdp+iccr+hiv+
           atmy+logufdth+hb2,data=newdata)
summary(m4) # AIC - 15575

###  polynominal  -  x-var에 th119^2, th119^3, th59^2, th59^3, logufdth^2, logufdth^2 추가 -  
th1192 = th119^2
th1193 = th119^3
th592 = th59^2
th593 = th59^3
logufdth2 = logufdth^2
logufdth3 = logufdth^3

full2 = glm(lfey~cAf+cAm+cAs+cEu+cOc+
             sts+
             achl+perex+bmi+gdp+th119+th1192+th1193+th59+th592+th593+
             iccr+sch+hiv+atmy+logufdth+logufdth2+logufdth3+hb2,
             data=newdata)

# polynominal model 1  Both direction by stepwise()
mp1 = step(base, scope=list(upper=full2, lower=~1), direction="both", trace=T)
summary(mp1)  # aic 15434

# polynominal model 2 Forward Selection by step()
mp2 = step(base, scope=list(upper=full2, lower=~1), direction="forward", trace=F) 
summary(mp2) # AIC: 15436

# polynominal model 3 Backward Elimination by step() based on AIC 
mp3= step(full2, direction = "backward", trace=TRUE) 
summary(mp3) # AIC:15425

# polynominal model 4 by best subset 
#install.packages("leaps")
library(leaps)
polyregsubsets.out <-
  regsubsets(lfey~cAf+cAm+cAs+cEu+cOc+
               sts+
               achl+perex+bmi+gdp+th119+th1192+th1193+th59+th592+th593+
               iccr+sch+hiv+atmy+logufdth+logufdth2+logufdth3+hb2,
             data = newdata,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(polyregsubsets.out)
as.data.frame(summary.out$outmat)
polyres.legend <-
  subsets(polyregsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
polyres.legend
which.max(summary.out$adjr2)  #20
summary.out$which[20,]
mp4 = glm(lfey~cAf+cAm+cOc+
            sts+
            achl+perex+bmi+th119+th1192+th1193+th59+th592+th593+
            iccr+sch+hiv+atmy+logufdth+logufdth2+hb2,data=newdata)
summary(mp4) # AIC - 15424 - best model.

#VIF TEST

