auto = read.csv("Auto.csv")
reg1 = lm(mpg ~ horsepower, data = auto)
summary(reg1)

predict(reg1, data.frame(horsepower = 98))


#b
plot(auto$mpg, auto$horsepower)
abline(reg1)
#c
par(mfrow = c(2,2))
plot(reg1)
par(mfrow = c(1, 1))


shapiro.test(resid(reg1))
