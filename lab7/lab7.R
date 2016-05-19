set.seed(33)
x <- matrix(rnorm(60*10), ncol = 10)
x
#generate
x[1:20, 1] = x[1:20, 1]+3
x[1:20, 2] = x[1:20, 2] +4
x[1:20, 3]= x[1:20, 3]+2
x[1:20, 4] = x[1:20, 4]+2
x[21:40, 1] = x[21:40, 1]-3
x[21:40, 2] = x[21:40, 2]-5
x[21:40, 3] = x[21:40, 3]-2
x[21:40, 4] = x[21:40, 4]-3

#library(mclust)
#fmm<-Mclust(x)
#fmm
#table(fmm$classification)

classification = c(rep(1,20),rep(2,20),rep(3,20))

#b
pr = princomp(x)
summary(pr)
pr$loadings
pr$scores
plot(pr$scores)

#c
set.seed(3)
km1 = kmeans(x, 3)
km1$cluster
plot(x, col = (km1$cluster + 1), main = "K-Means K=3", xlab = "", ylab = "", pch = 20, cex = 2)
table(classification, km1$cluster)

#d
set.seed(4)
km2 = kmeans(x, 2)
plot(x, col = (km2$cluster + 1), main = "K-Means K=2", xlab = "", ylab = "", pch = 20, cex = 2)
table(classification, km2$cluster)
#e
set.seed(5)
km3 = kmeans(x, 4)
plot(x, col = (km3$cluster + 1), main = "K-Means K=4", xlab = "", ylab = "", pch = 20, cex = 2)
table(classification, km3$cluster)
#f
set.seed(6)
km4 = kmeans(pr$scores, 3)
plot(pr$scores, col = (km4$cluster + 1), main = "K-Means, K=3", xlab = "", ylab = "", pch = 20, cex = 2)
table(classification, km4$cluster)
#g
set.seed(7)
t=scale(x, scale=TRUE, center = FALSE)
sd(t[,2])
sd(t[,3])
sd(t[,4])
var(t[,1])
mean(t[,2])
km5 =  kmeans(t, 3)
plot(x, col = (km5$cluster + 1), main = "K-Means, K=3", xlab = "", ylab = "", pch = 20, cex = 2)
