library(ISLR)
#Ex.1
?Weekly
summary(Weekly)
pairs(Weekly)

l1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          data = Weekly, family = binomial)
summary(l1)

#c
probs = predict(l1, type="response")
probs[1:5]
contrasts(Weekly$Direction)
pred = ifelse(probs > 0.5, "Up", "Down")
table(pred, Weekly$Direction)
mean(pred == Weekly$Direction)

#d
cond = Weekly$Year <= 2008
l1 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = cond)
probs = predict(l1, newdata = Weekly[!cond,], type="response") 
pred = ifelse(probs > 0.5, "Up", "Down")
Direction.2009_10 = Weekly$Direction[!cond]
table(pred, Direction.2009_10)
mean(pred == Direction.2009_10)

#e Lag2+Lag4
cond = Weekly$Year <= 2008
l1 = glm(Direction ~ Lag2+ Lag4, data = Weekly, family = binomial, subset = cond)
probs = predict(l1, newdata = Weekly[!cond,], type="response") 
pred = ifelse(probs > 0.5, "Up", "Down")
Direction.2009_10 = Weekly$Direction[!cond]
table(pred, Direction.2009_10)
mean(pred == Direction.2009_10)

#Ex2.

auto = read.csv("Auto.csv")
med = median(auto$mpg)
med
mpg01 = ifelse(auto$mpg > med, 1, 0)
mpg01
auto = data.frame(auto, mpg01)
pairs(auto)

#200
lrnSet = sample(nrow(auto),200) 
l1 = glm(mpg01 ~ displacement + horsepower + weight + acceleration,
          data = auto, family = binomial, subset = lrnSet)
summary(l1)
probs = predict(l1, newdata = auto[-lrnSet,],type = "response")
pred = ifelse(probs > 0.5, 1, 0)
#pred
mean(pred == mpg01[-lrnSet])
k = table(pred, mpg01[-lrnSet])
k
k[1,1]/sum(k[,1])
k[1,1]/sum(k[,2])

#100
lrnSet = sample(nrow(auto),100) 
l1 = glm(mpg01 ~ displacement + horsepower + weight + acceleration,
         data = auto, family = binomial, subset = lrnSet)
summary(l1)
probs = predict(l1, newdata = auto[-lrnSet,],type = "response")
pred = ifelse(probs > 0.5, 1, 0)
#pred
mean(pred == mpg01[-lrnSet])
k = table(pred, mpg01[-lrnSet])
k
k[1,1]/sum(k[,1])
k[1,1]/sum(k[,2])


