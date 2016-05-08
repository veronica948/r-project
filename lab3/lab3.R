#Ex 1
set.seed(13)
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
abline(l1, lwd = 1, col = "blue")
legend(x = 1, y = -1.5,"l1", text.col = "blue")
confint(l1)
#2.5 %     97.5 %
#  (Intercept) -1.0691240 -0.9662907
#x            0.4074343  0.5197821
#-------------------------------------

eps = rnorm(n, 0, 0.05)
y = -1 + 0.5*x + eps
length(y) #100
#b0 = -1; b1 = 0.5
plot(y ~ x)
l1 = lm(y ~ x)
summary(l1)
l1$coefficients  # -1.0020461   0.4979084

plot(y~x)
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
l1$coefficients  # -1.1227244   0.3301253

plot(y~x)
points(y~x)
abline(l1, lwd = 1, col = "blue")
legend(x = 1, y = -2,"l1", text.col = "blue")
confint(l1)

#Ex 2
library(MASS)
boston = Boston
?Boston

a_coeffs = vector(length = 13)

#zn
l1 = lm(crim ~ zn, data = boston)
summary(l1)
l1$coefficients
a_coeffs[1] = l1$coefficients[2]
plot(boston$crim~boston$zn)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(zn,3), data = boston)
l1$coefficients
summary(l1)


#indus
l1 = lm(crim ~ indus, data = boston)
summary(l1)
l1$coefficients # -2.0637426   0.5097763 
a_coeffs[2] = l1$coefficients[2]
plot(boston$crim~boston$indus)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(indus,3), data = boston)
l1$coefficients
summary(l1)

#chas
l1 = lm(crim ~ chas, data = boston)
summary(l1)
l1$coefficients
a_coeffs[3] = l1$coefficients[2]
plot(boston$crim~boston$chas)
abline(l1, lwd = 1, col = "blue")

#всего два значения


#nox
l1 = lm(crim ~ nox, data = boston)
summary(l1)
l1$coefficients
a_coeffs[4] = l1$coefficients[2]
plot(boston$crim~boston$nox)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(nox,3), data = boston)
l1$coefficients

#rm 
l1 = lm(crim ~ rm, data = boston)
summary(l1)
l1$coefficients
a_coeffs[5] = l1$coefficients[2]
plot(boston$crim~boston$rm)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(rm,3), data = boston)
l1$coefficients
summary(l1)

#age
l1 = lm(crim ~ age, data = boston)
summary(l1)
l1$coefficients
a_coeffs[6] = l1$coefficients[2]
plot(boston$crim~boston$age)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(age,3), data = boston)
l1$coefficients
summary(l1)

#dis
l1 = lm(crim ~ dis, data = boston)
summary(l1)
l1$coefficients
a_coeffs[7] = l1$coefficients[2]
plot(boston$crim~boston$dis)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(dis,3), data = boston)
l1$coefficients
summary(l1)

#rad

l1 = lm(crim ~ rad, data = boston)
summary(l1)
l1$coefficients
a_coeffs[8] = l1$coefficients[2]
plot(boston$crim~boston$rad)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(rad,3), data = boston)
l1$coefficients
summary(l1)

#tax
l1 = lm(crim ~ tax, data = boston)
summary(l1)
l1$coefficients
a_coeffs[9] = l1$coefficients[2]
plot(boston$crim~boston$tax)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(tax,3), data = boston)
l1$coefficients
summary(l1)

#ptratio

l1 = lm(crim ~ ptratio, data = boston)
summary(l1)
l1$coefficients
a_coeffs[10] = l1$coefficients[2]
plot(boston$crim~boston$ptratio)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(ptratio,3), data = boston)
l1$coefficients
summary(l1)

#black

l1 = lm(crim ~ black, data = boston)
summary(l1)
l1$coefficients
a_coeffs[11] = l1$coefficients[2]
plot(boston$crim~boston$black)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(black,3), data = boston)
l1$coefficients
summary(l1)

#lstat
l1 = lm(crim ~ lstat, data = boston)
summary(l1)
l1$coefficients
a_coeffs[12] = l1$coefficients[2]
plot(boston$crim~boston$lstat)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(lstat,3), data = boston)
l1$coefficients
summary(l1)

#medv

l1 = lm(crim ~ medv, data = boston)
summary(l1)
l1$coefficients
a_coeffs[13] = l1$coefficients[2]
plot(boston$crim~boston$medv)
abline(l1, lwd = 1, col = "blue")

l1 = lm(crim ~ poly(medv,3), data = boston)
l1$coefficients
summary(l1)


l1 = lm(crim ~ ., data = boston)
summary(l1)
b_coeffs = l1$coefficients[2:14]
coeffs = data.frame(x = a_coeffs, y = b_coeffs)
coeffs
plot(coeffs, xlim = c(-5,5), ylim = c(-5,5))