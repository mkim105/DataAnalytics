###Describe Quantitative Data###

# set environment and load data into R
setwd("C:/Users/user/Desktop/Data Analytics/") # set working directory = setwd
data = read.table("Case1_Student Grades_Regular.csv",header=T, sep=',')

# get a summary of the data
str(data) # structure(data)

# Let's focus on the variable Grade
g = data$Grade

# note that if you have missing values in it, you cannot produce numerical metrics
# you need to use na.omit to ignore the missing values
summary(g)

# use describe function in package 'psych'
install.packages('psych')
library(psych)
describe(g)
# 관측값개수, 평균, 표준오차, 중앙값, 절삭평균, 평균절대편차, 최소, 최대, 범위
# 왜도, 첨도, 표준오차, vars => 그냥 해당 class에 붙이는 넘버.
# kurtosis : 첨도 : 얼마나 뾰족한지.
# 절삭평균(trimmed mean)은 평균값과 중앙값을 절충하기 위한 것.
# 자료로부터 가장 큰 값과 가장 작은 값들을 제거하고, 나머지 관측치들의 평균값을 계산

# histogram
hist(g)

# boxplot to compare Age, Grade and Exam
# prepare your data
values=data[,c('Age','Exam','Grade')]
boxplot(values,col=rainbow(ncol(values)))

# plot histogram with normal curve
summary(g)
h<-hist(g, main="Histogram with Normal Curve")
xfit<-seq(min(g),max(g),length=40) # seq()는 sequence의 줄임말. 순차적인 데이터 생성
# 즉 min부터 max까지 40개의 값을 생성해라.
yfit<-dnorm(xfit,mean=mean(g),sd=sd(g)) # normal distribution (x,평균,표준편차)
yfit<-yfit*diff(h$mids[1:2])*length(g) # 모르겠다
lines(xfit,yfit,col="blue",lwd=2)