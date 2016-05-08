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

View(x)
library(mclust)
fmm = Mclust(x)
fmm
fmm$classification
table(fmm$classification)

#b
pr.out = prcomp(x, scale = TRUE)
pr.out$center #ñðåäíåå
pr.out$scale #äèñïåðñèÿ
pr.out$rotation #ìàòðèöà, êîò. ñîñò. èç âåêòîðîâ íàãðóçîê
biplot(pr.out)
plot(x, col = (fmm$classification + 1), main = "pc", xlab = "", ylab = "", pch = 20, cex = 2)

#c
set.seed(3)
km.out = kmeans(x, 3, nstart = 20)
km.out$cluster
plot(x, col = (km.out$cluster + 1), main = "K-Means K=3", xlab = "", ylab = "", pch = 20, cex = 2)
km.out
km.out$cluster
km.out$tot.withinss
table(fmm$classification, km.out$cluster)

#d
set.seed(4)
km.out = kmeans(x, 2, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = "K-Means K=2", xlab = "", ylab = "", pch = 20, cex = 2)
table(fmm$classification, km.out$cluster)
#e
set.seed(5)
km.out = kmeans(x, 4, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=4", xlab = "", ylab = "", pch = 20, cex = 2)
table(fmm$classification, km.out$cluster)
#f
set.seed(6)
km.out = kmeans(pr.out$x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = "K-Means, K=3", xlab = "", ylab = "", pch = 20, cex = 2)
table(fmm$classification, km.out$cluster)
#g
set.seed(7)
t=scale(x, center = TRUE)
t
sd(t[,1])
km.out <- kmeans(t, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = "K-Means, K=3", xlab = "", ylab = "", pch = 20, cex = 2)
