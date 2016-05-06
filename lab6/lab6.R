library(ISLR)
library(MASS)
summary(Weekly)
#LDA
trainCond = (Weekly$Year <= 2008)
l1 = lda(Direction ~ Lag2, data = Weekly, subset = trainCond)
l1

pred = predict(l1, Weekly[!trainCond, ])
lda.class = pred$class
testDirection = Weekly$Direction[!trainCond]
table(lda.class, testDirection)
mean(lda.class == testDirection)


trainCond = (Weekly$Year <= 2008)
l1 = qda(Direction ~ Lag2, data = Weekly, subset = trainCond)
l1
pred = predict(l1, Weekly[!trainCond, ])
qda.class = pred$class
testDirection = Weekly$Direction[!trainCond]
table(qda.class, testDirection)
mean(qda.class == testDirection)
#glm 0.625
#lda 0.625
#qda 0.625


#Ex2

#Ex2.

auto = read.csv("Auto.csv")
med = median(auto$mpg)
med
mpg01 = ifelse(auto$mpg > med, 1, 0)
mpg01
auto = data.frame(auto, mpg01)
pairs(auto)
plot(mpg01~auto$displacement)
plot(mpg01~auto$year)
#mpg, displacement, horsepower, weight, acceleration
cond = auto$year <= 78
l1 = lda(mpg01 ~ displacement + horsepower + weight + acceleration,
         data = auto, family = binomial, subset = cond)
l1
pred = predict(l1, auto[!cond, ])
lda.class = pred$class
test = mpg01[!cond]
table(lda.class, test)
mean(lda.class == test)

q1 = qda(mpg01 ~ displacement + horsepower + weight + acceleration,
         data = auto, family = binomial, subset = cond)
q1
pred = predict(q1, auto[!cond, ])
qda.class = pred$class
test = mpg01[!cond]
table(qda.class, test)
mean(qda.class == test)
