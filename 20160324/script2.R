x = seq(0,1,0.001)

data = data.frame(x^2)

data$y = data$x + rnorm(nrow(data), 0, 0.5)

pairs(data)

#reg1 = lm(y~x, data)
reg2 = lm(y~-1 + x, data)
reg2$coefficients
b = 0.03977
a = 0.92260
data$y - (a * data$x + b)

y2 = predict(reg2, data)

res = data$y - y2

cor(data$x, res)
