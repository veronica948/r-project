auto = read.csv("Auto.csv")
summary(auto)
reg1 = lm(mpg ~ horsepower,auto)
summary(reg1)

reg1$coefficients

s = subset(auto,auto$horsepower == 98)
y2 = predict(reg1, s)
print(y2)


#b
plot(auto$horsepower, auto$mpg)
points(auto$horsepower, auto$mpg)
abline(reg1,col="red")

#c
par(mfrow = c(2,2))
plot(reg1)
par(mfrow = c(1, 1))

