#pr = princomp   principal component analysis
# predict()

library(ISLR)

?Carseats
summary(Carseats)
pr = princomp(~Price + Sales +Age, Carseats)
summary(pr)
pr$loadings
predict(pr, Carseats)


cov()
eigen()