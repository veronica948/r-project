x = seq(0,1,0.001)

data = data.frame(x)

data$y = data$x + rnorm(nrow(data), 0, 0.5)

md = glm(y~x + x^2, family = gaussian, data)
md
confint(md)
res = predict(md, data)
rr = residuals(md)
plot(rr)
#pairs(data)
#plot(data)
#points(data)
#abline(0,1,col="red")

#reg1 = lm(y~x, data)
#lm(y~-1 + x, data)
#reg1$coefficients
#b = 0.03977
#a = 0.92260
#data$y - (a * data$x + b)

#y2 = predict(reg1, data)
#res = data$y - y2

#cor(data$x, res)glm(y)