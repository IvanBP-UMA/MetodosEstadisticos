library(tidyverse)

###############
# Ejercicio 1 #
###############

series <- ts(c(9.47, 9.26, 8.86, 8.25, 7.81, 8.01, 7.55, 7.24, 7.01, 6.88, 7.03), start=c(1973, 1), frequency = 1)

#Apartado a
tend4 <- stats::filter(series, c(0.5, 1, 1, 1, 0.5) / 4)
tend4
tend5 <- stats::filter(series, c(1, 1, 1, 1, 1)/5)
plot(series)
lines(tend4, col='red')
lines(tend5, col='blue')

#Apartado b
aux <- 1:11
mod <- lm(series ~ aux)
plot(aux, series)
abline(mod)

###############
# Ejercicio 3 #
###############

series <- ts(c(3.9, 4, 4.8, 5.1, 5, 5.5, 6.1, 6.3, 6.9), start = c(2016, 1), frequency = 3)
tend <- stats::filter(series, c(1, 1, 1)/3)
plot(series)
lines(tend)

est_ale <- series/tend
estInd <- colMeans(matrix(est_ale, ncol = 3, byrow = T), na.rm = T)
estNormalized = estInd/mean(estInd)
estTS = ts(rep(estNormalized, 3), start = c(2016,1), frequency = 3)
ale = series/estTS

seriesDesest = series/estTS
plot(seriesDesest)

#Metodo rapido
dc <- decompose(series, type='mul')
series / dc$seasonal

###############
# Ejercicio 4 #
###############

dat <- c(178.2, 156.7, 164.2, 153.2, 157.5, 172.6, 185.9, 185.8, 165, 163.6, 169, 183.1,
         196.3, 162.8, 168.6, 156.9, 168.2, 180.2, 197.9, 195.9, 176, 166.4, 166.3, 183.9,
         197.3, 173.7, 173.2, 159.7, 175.2, 187.4, 202.6, 205.6, 185.6, 175.6, 176.3, 191.7,
         209.5, 186.3, 183, 169.5, 178.2, 186.7, 202.4, 204.9, 180.6, 179.8, 177.4, 188.9,
         200, 188.7, 187.5, 168.6, 175.7, 189.4, 216.1, 215.4, 191.5, 178.5, 178.6, 195.6,
         205.2, 179.6, 185.4, 172.4, 177.7, 202.7, 220.2, 210.2, 186.9, 181.4, 175.6, 195.6)
series <- ts(dat, start = c(1976, 1), frequency = 12)

#Apartado a
tend <- stats::filter(series, c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5)/12)
est_ale <- series/tend
est <- colMeans(matrix(est_ale, ncol = 12, byrow = T), na.rm = T)
estNormalized <- est / mean(est)

#Apartado b
seriesDesest <- series / estNormalized

#Apartado c
plot(series, col='red')
lines(seriesDesest, col='blue')

#Apartado d
aux <- seq_along(series)
mod <- lm(seriesDesest~aux)
tendReal <- ts(predict.lm(mod), start = c(1976, 1), frequency = 12)
decompose(series, type = 'mul')

#Apartado e
ale <- est_ale/estNormalized
ciclic <- tend/tendReal
ciclic
