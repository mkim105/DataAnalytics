setwd('C:/users/mg/Desktop/Data Analytics/HW/HW7/missingvaluepractice')
getwd()

# Y = consume variable
# K와 L은 빈칸이 너무 많아서 지워버린다.
# F의 경우 1. 너무 많은 missing values, 2. Nominal variable이고 이것은 이미 H,I,J에 의해 표현되었다.

data = read.csv("measurements.csv", header=T)
head(data)

mydata = data[,c(1:5, 7:10)] # F,K,L을 제외한 columns을 선택한다.
head(mydata)  

#install.packages("dummies") #if necessary
library(dummies)

mydata = dummy.data.frame(mydata,names=c("gas_type")) #gas_type을 2개의 dummy variables로 만든다.(gas_typeE10 and gas_typeSP98)
head(mydata)

summary(mydata) # Missing values을 찾을 수 있다.

# temp_inside의 missing values 제거
mydata$temp_inside = ifelse(is.na(mydata$temp_inside), ave(data$temp_inside, FUN = function(x) mean(x, na.rm=T)), mydata$temp_inside)

head(mydata)

summary(mydata)       #여기서 확인가능

nrow(mydata) # How many rows in my data? >> 388 >> data is small , so we should use N-fold eval

cor(mydata) # 왜 에러뜨지

# 모델 만들기 
full =glm(consume ~ distance + speed + temp_inside + temp_outside + AC + rain + sun + gas_typeE10 + gas_typeSP98, data = mydata)
base = glm(consume ~ distance, data = mydata )
m1 = step(base, scope = list(upper=full, lower=~1), direction = "both", trace = F)


# residual analysis
res = rstandard(m1)
plot( fitted(m1), res, main = "Predicted vs residuals plot")
abline(a=0,b=0,col='red')
