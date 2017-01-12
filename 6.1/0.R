x <- -10:10
par(mfrow=c(3, 2))

y <- 3*x + 1
plot(x, y, type = "l")

y <- -3*x +1
plot(x, y, type = "l")

y <- 3^x
plot(x, y, type = "l")

y <- -3^x
plot(x, y, type = "l")

y <- 3*x^2 + 3*x + 2
plot(x, y, type = "l")

y <- -3*x^2 + 3*x + 2
plot(x, y, type = "l")
