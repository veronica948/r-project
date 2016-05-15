library(ISLR)
library(MASS)
summary(Weekly)
#LDA
trainCond = (Weekly$Year <= 2008)
l1 = lda(Direction ~ Lag2, data = Weekly, subset = trainCond)
l1

pred = predict(l1, Weekly[!trainCond, ])
class = pred$class
class
testDirection = Weekly$Direction[!trainCond]
table(class, testDirection)
mean(class == testDirection)

trainCond = (Weekly$Year <= 2008)
l1 = qda(Direction ~ Lag2, data = Weekly, subset = trainCond)
l1
pred = predict(l1, Weekly[!trainCond, ])
qdaClass = pred$class
testDirection = Weekly$Direction[!trainCond]
table(qdaClass, testDirection)
mean(qdaClass == testDirection)
#glm 0.625
#lda 0.625
#qda 0.5865385

#Ex2.

auto = read.csv("Auto.csv")
med = median(auto$mpg)
med
mpg01 = ifelse(auto$mpg > med, 1, 0)
mpg01
auto = data.frame(auto, mpg01)
pairs(auto)
#mpg, displacement, horsepower, weight, acceleration
cond = auto$year <= 78
l1 = lda(mpg01 ~ displacement + horsepower + weight + acceleration,
         data = auto, family = binomial, subset = cond)
l1
pred = predict(l1, auto[!cond, ])
ldaClass = pred$class
ldaClass
test = mpg01[!cond]
table(ldaClass, test)
mean(ldaClass == test) #0.877193

q1 = qda(mpg01 ~ displacement + horsepower + weight + acceleration,
         data = auto, family = binomial, subset = cond)
q1
pred = predict(q1, auto[!cond, ])
qdaClass = pred$class
test = mpg01[!cond]
table(qdaClass, test)
mean(qdaClass == test) #0.8421053
