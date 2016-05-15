library(ISLR)
?Carseats
l1 = lm(Sales~Price+Urban+US, Carseats);
summary(l1)
#y = 13.043469 -0.054459Price -0.021916Urban + 1.200573Us + eps
par(mfrow = c(2,2))
plot(l1)
l2 = lm(Sales~Price+US, Carseats);
summary(l2)
plot(l2)
par(mfrow = c(1, 1))
confint(l2)

#Ex2
set.seed(3)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3 *x2 + rnorm(100)
#b0=2; b1=2; b2=0.3
cor(x1,x2) #0.8425111
plot(x1~x2)
l3 = lm(y ~ x1 + x2)
summary(l3)
#b1 =  3.1700  b2 = -0.6851
l4 = lm(y ~ x1)
summary(l4)
plot(y~x1)
abline(l4,col="red")
#b1 = 1.4883
l5 = lm(y ~ x2)
summary(l5)
plot(y~x2)
abline(l5,col="red")
#b2 = 3.7969 
par(mfrow = c(2,2))
plot(l3)
plot(l4)
plot(l5)
par(mfrow = c(1,1))

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
#"Влиятельное" наблюдение, если оно опущено, изменяет одну 
#или больше оценок параметров модели (т.е. угловой коэффициент 
#или свободный член). 
#Выброс (наблюдение, которое противоречит большинству значений в 
#наборе данных) может быть "влиятельным" наблюдением и может хорошо обнаруживаться визуально, 
#при осмотре двумерной диаграммы рассеяния или графика остатков. 

l3 = lm(y ~ x1 + x2)
summary(l3)
#b1 =  1.8782 b2 =  1.6442 
l4 = lm(y ~ x1)
summary(l4)
plot(y~x1)
abline(l4,col="red")
#b1 = 2.6467
l5 = lm(y ~ x2)
summary(l5)
plot(y~x2)
abline(l5,col="red")
#b2 = 3.9160 

par(mfrow = c(2,2))
plot(l3)
plot(l4)
plot(l5)
par(mfrow = c(1,1))
