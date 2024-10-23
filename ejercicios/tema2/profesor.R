x <- c(1, 2, 3, 4, 5)
y <- c(3, 4.5, 7, 10, 15)
mod1  <- lm(y~x)
coef1 <- mod1$coefficients
summary(mod1)$r.squared

z <- log(y)
mod2  <- lm(z~x)
coef2 <- exp(mod2$coefficients)
# OJO: Este valor no es el que hay que utilizar. Es el R2 de z. NO nos vale
summary(mod2)$r.squared

# y predichas por el modelo 1 (dos maneras eq de calcularlo)
predict.lm(mod1)
yp1 <- coef1[1] + coef1[2] * x

# y predichas por el modelo 2
exp(predict.lm(mod2))
yp2 <- coef2[1] * coef2[2] ^ x

# R2 
MSE1 <- mean((y - yp1)^2)
MSE2 <- mean((y - yp2)^2)
varY <- mean(y^2) - mean(y)^2

R2_1 <- 1 - MSE1/varY
R2_2 <- 1 - MSE2/varY
