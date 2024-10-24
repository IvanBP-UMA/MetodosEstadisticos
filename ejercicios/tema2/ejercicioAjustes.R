#Ejercicio 8
x <- c(1, 2, 3, 4, 5)
y <- c(3, 4.5, 7, 10, 15)

z <- log(y)
mod1 <- lm(z~x)
yEst <- exp(predict.lm(mod1))
MSE <- mean((y - yEst)^2)
varY <- mean(y^2) - mean(y)^2

rSQRD <- 1 - MSE/varY
summary(mod1)$r.squared

################
#Ejercicio 11
################
x <- c(1, 2, 3, 4, 5)
y <- c(1, 1, 2, 4, 8)

modLineal <- lm(y~x)
yp <- log(y)
modExp <- lm(yp~x)
exp(predict.lm(modExp))

MSELineal = mean((y-predict.lm(modLineal))^2)
MSEExp = mean((y-exp(predict.lm(modExp)))^2)
varY = mean(y^2) - mean(y)^2

#Es mejor el modelo exponencial

x2 <- c(6, 10)
yLineal <- modLineal$coefficients[1] + modLineal$coefficients[2]*x2
yExp <- exp(modExp$coefficients[1] + modExp$coefficients[2]*x2)
yExp <- exp(modExp$coefficients[1])*exp(modExp$coefficients[2])^x2
