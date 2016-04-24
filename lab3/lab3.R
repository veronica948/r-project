#Ex 1
set.seed(1597)
n = 100
x = rnorm(n, 0, 1)
eps = rnorm(n, 0, 0.25)
y = -1 + 0.5*x + eps
length(y) #100
#b0 = -1; b1 = 0.5
plot(y ~ x)
l1 = lm(y ~ x)
summary(l1)
l1$coefficients  #-1.0177073   0.4636082 

plot(y~x)
points(y~x)
abline(l1, lwd = 1, col = "blue")
legend(x = 0, y = -1.5,"l1", text.col = "blue")
confint(l1)

#-------------------------------------

eps = rnorm(n, 0, 0.05)
y = -1 + 0.5*x + eps
length(y) #100
#b0 = -1; b1 = 0.5
plot(y ~ x)
l1 = lm(y ~ x)
summary(l1)
l1$coefficients  #-1.0177073   0.4636082 

plot(y~x)
points(y~x)
abline(l1, lwd = 1, col = "blue")
legend(x = 0, y = -1.5,"l1", text.col = "blue")
confint(l1)

#-----------------------
eps = rnorm(n, 0, 0.75)
y = -1 + 0.5*x + eps
length(y) #100
#b0 = -1; b1 = 0.5
plot(y ~ x)
l1 = lm(y ~ x)
summary(l1)
l1$coefficients  #-1.0177073   0.4636082 

plot(y~x)
points(y~x)
abline(l1, lwd = 1, col = "blue")
legend(x = 0, y = -1.5,"l1", text.col = "blue")
confint(l1)

#Ex 2