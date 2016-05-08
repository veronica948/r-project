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
cor(x1,x2) #0.7837236
plot(x1~x2)
l3 = lm(y ~ x1 + x2)
summary(l3)
#b1 =  1.3203  b2 = 0.3771
l4 = lm(y ~ x1)
summary(l4)
plot(y~x1)
abline(l4,col="red")
#b1 = 1.4883
l5 = lm(y ~ x2)
summary(l5)
plot(y~x2)
abline(l5,col="red")
#b2 = 2.1969 
par(mfrow = c(2,2))
plot(l3)
plot(l4)
plot(l5)
par(mfrow = c(1,1))

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
#Наблюдение представлено 
#необычным сочетанием значений предикторов (англ. leverage point).
#Наблюдение не согласуется с рассматриваемой моделью, 
#т.е. является выбросом (англ. outlier).

l3 = lm(y ~ x1 + x2)
summary(l3)
#b1 = 0.6031 b2 = 1.7499 
l4 = lm(y ~ x1)
summary(l4)
plot(y~x1)
abline(l4,col="red")
#b1 = 1.3180
l5 = lm(y ~ x2)
summary(l5)
plot(y~x2)
abline(l5,col="red")
#b2 = 2.4587

par(mfrow = c(2,2))
plot(l3)
plot(l4)
plot(l5)
par(mfrow = c(1,1))
