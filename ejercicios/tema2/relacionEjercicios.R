library(tidyverse)

#Ejercicio 3#
dat <- tibble(x=c(1, 2, 3, 4, 5), y1=c(4, 2, 3, 2, 4), y2=c(1, 3, 5, 7, 9))

covXY1 <- mean(dat$x*dat$y1) - mean(dat$x)*mean(dat$y1)
covXY2 <- mean(dat$x*dat$y2) - mean(dat$x)*mean(dat$y2)

lm(y1~x, dat)
lm(y2~x, dat)

#Ejercicio 4
dat <- tibble(x=c(43, 55, 40, 52, 39, 33, 50, 33, 44, 21), y=c(27, 23.8, 30.7, 24, 34.8, 41.4, 27, 40.4, 31.7, 51.2))

varX <- mean(dat$x^2) - mean(dat$x)^2
varY <- mean(dat$y^2) - mean(dat$y)^2
covXY <- mean(dat$x*dat$y) - mean(dat$x)*mean(dat$y)

mod <- lm(y~x, dat)
sqrt(summary(mod)$r.squared)
