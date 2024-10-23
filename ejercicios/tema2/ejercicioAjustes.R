x <- c(1, 2, 3, 4, 5)
y <- c(3, 4.5, 7, 10, 15)

z <- log(y)
mod1 <- lm(z~x)
yEst <- exp(predict.lm(mod1))
MSE <- mean((y - yEst)^2)
varY <- mean(y^2) - mean(y)^2

rSQRD <- 1 - MSE/varY
summary(mod1)$r.squared
