###Describe Qualitative Data###

# set environment and load data into R
setwd("C:/Users/user/Desktop/Data Analytics/") # set working directory = setwd
data = read.table("Case1_Student Grades_Regular.csv",header=T, sep=',')

# get a summary of the data
str(data) # structure(data)

# let's focus on the variable Nationality
nat = data$Nationality
class(nat) # 추상적인 자료형, factor(Nominal) 이다..
mode(nat) # 물리적으로 메모리에 저장되는 방식. numeric. e.g) 중국 = 1
typeof(nat) # nat은 numeric으로 저장되고, 그 타입은 integer(정수) 이다.

# install and load library plyr
install.packages('plyr')
library(plyr)

# call the function count in plyr
count(nat) # 각 나라가 얼마나 등장하는가 빈도수를 알아냄.
# 즉 count(nat)는 class frequency를 나타냄.

# if you want to get class relative frequency
table(data$Nationality)/nrow(data) #crf
table(data$Nationality) #cf

# prepare the data for plots
opt=count(nat)
cf=opt$freq 
labels=opt$x
crf=table(data$Nationality)/nrow(data)

# produce Pie Chart : qualitative -> crf
pie(crf,labels)

# produce bar graph by using class frequency : qualitative -> cf
barplot(cf,names.arg=labels)


