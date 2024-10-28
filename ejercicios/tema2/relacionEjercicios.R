library(tidyverse)

###############
# Ejercicio 3 #
###############

dat <- tibble(x=c(1, 2, 3, 4, 5), y1=c(4, 2, 3, 2, 4), y2=c(1, 3, 5, 7, 9))

covXY1 <- mean(dat$x*dat$y1) - mean(dat$x)*mean(dat$y1)
covXY2 <- mean(dat$x*dat$y2) - mean(dat$x)*mean(dat$y2)

lm(y1~x, dat)
lm(y2~x, dat)


###############
# Ejercicio 4 #
###############

dat <- tibble(x=c(43, 55, 40, 52, 39, 33, 50, 33, 44, 21), y=c(27, 23.8, 30.7, 24, 34.8, 41.4, 27, 40.4, 31.7, 51.2))

varX <- mean(dat$x^2) - mean(dat$x)^2
varY <- mean(dat$y^2) - mean(dat$y)^2
covXY <- mean(dat$x*dat$y) - mean(dat$x)*mean(dat$y)

mod <- lm(y~x, dat)
sqrt(summary(mod)$r.squared)
covXY/(sqrt(varX)*sqrt(varY))

################
# Ejercicio 13 #
################

C <- c(2, 4, 6, 8, 10, 12)
E <- c(10, 19, 29, 40, 48, 56)
mod <- lm(E~C + 0)


################
# Ejercicio 14 #
################

x <- c(5, 5, 5, 5, 10, 10, 10, 10, 15, 15, 15, 15, 20, 20, 20, 20)
y <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
z <- c(28, 30, 48, 74, 29, 50, 57, 42, 20, 24, 31, 47, 9, 18, 22, 31)

mod <- lm(z~x+y)
zest <- predict.lm(mod)

varZ <- mean(z^2) - mean(z)^2
MSE <- mean((z-zest)^2)
R2 <- 1- MSE/varZ
