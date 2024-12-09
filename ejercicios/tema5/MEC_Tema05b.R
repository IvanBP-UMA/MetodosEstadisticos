##############################################################
# Métodos estadísticos para la computación
# Escuela Técnica Superior de Ingeniería Informática.
# Universidad de Málaga. Curso 2020 / 21
# Tema 5b. Distribuciones estándar
##############################################################
library(tidyverse)

##############################################################
# Distribuciones discretas
##############################################################
##############################################################
# Distribución Uniforme discreta
##############################################################
# Generación números aleatorios uniforme discreta
# 600 tiradas de dados
xx <- sample(1:6, 600, replace=T)
# frecuencia
table(xx)
hist(xx)


##############################################################
# Distribución Binomial
##############################################################
# Distribución binomial
# Simulación de 10000 tiradas de 10 monedas
# Cada elemento sería el número de caras que ha salido en cada tirada
xx <- rbinom(10000, 10, .5)
# ¿cuantas veces han salido 10 caras?
sum(xx == 10)

table(xx)
hist(xx)

# Función de probabilidad
# Probabilidad de sacar 1 cara en 10 tiradas de monedas
dbinom(1, 10, .5)
# Probabilidad de sacar 5 caras en 10 tiradas de monedas
dbinom(5, 10, .5)

# Función de distribución
# Probabilidad de sacar 1 cara o menos
pbinom(1, 10, .5)
# es lo mismo que:
dbinom(0, 10, .5) + dbinom(1, 10, .5)
# Probabilidad de sacar 10 caras o menos. Tiene que valer 1.
pbinom(10, 10, .5)

# Función cuantil:
# Mediana
qbinom(.5, 10, .5)
# Percentil 10
qbinom(.1, 10, .5)

# Ejemplo de dualidad
# Es lo mismo 3 éxitos de 10 con prop = 0.7
dbinom(3, 10, .7)
# que 7 éxitos de 10 con prop = 0.3
dbinom(7, 10, .3)

#Ejemplo diapositiva 11
# Apartado 1
dbinom(2, 15, .02)
# Apartado 2
1 - pbinom(1, 15, .02)
1 - (dbinom(0, 15, .02) + dbinom(1, 15, .02))
pbinom(15, 15, .02) - pbinom(1, 15, .02)
# Apartado 3
dbinom(1, 4, .02)


##############################################################
# Distribución Geométrica
##############################################################
# OJO: En las diapositivas definimos la dist geométrica como el punto donde aparece el primer éxito
# En R es el número de intentos PREVIOS al éxito
dgeom(0, .98)
dgeom(1, .98)

# Ejemplo Diapositiva 13
1 - pgeom(2, .98)

# Función de probabilidad equivalente a dgeom
dgeom1 <- function(x, p) p * (1 - p) ^ x
# Función de probabilidad como la definimos en las diapositivas
dgeom2 <- function(x, p) p * (1 - p) ^ (x - 1)

dgeom1(0, .98)


##############################################################
# Distribución de Poisson
##############################################################
# Probabilidad de 10 sucesos si la media es 10
dpois(10, 10)
# Probabilidad de 10 sucesos o menos si la media es 10
ppois(10, 10)
# Aquí vemos que el 30% de las veces ocurren 8 sucesos o menos (si la media es 10)
qpois(.3, 10)



##############################################################
# Distribuciones continuas
##############################################################
##############################################################
# Distribución Uniforme continua
##############################################################
# Simulamos 1000 valores uniformes entre 10 y 20
runif(1000, 10, 20) %>% floor %>% table
# Representamos la función densidad uniforme U[10, 20]
x <- seq(0, 30, length.out = 101)
y <- dunif(x, 10, 20)

plot(x, y, type="l")
# Probabilidad del intervalo (12, 15)
punif(15, 10, 20) - punif(12, 10, 20)
# Percentil 10
qunif(.1, 10, 20)


##############################################################
# Distribución normal
##############################################################
# Simulamos 1000 sujetos de una población de altura media 175 y desv 10
xx <- rnorm(1000, 175, 10)
min(xx)
max(xx)
xx %>% floor %>% table
# Representamos la densidad
x <- seq(140, 210, length.out=200)
y <- dnorm(x, 175, 10)
plot(x, y, type="l")

# ¿Qué proporción mide más de 185
1 - pnorm(185, 175, 10)

# el 10% más alto a partir de qué altura está?
qnorm(.9, 175, 10)

# Simetría: Hay los mismo sujetos por debajo de 165, que por encima de 185
pnorm(165, 175, 10)
1 - pnorm(185, 175, 10)

# Individuos entre mu-sigma y mu+sigma
# Siempre hay 0.6827, independientemente de los valores de mu y sigma
pnorm(185, 175, 10) - pnorm(165, 175, 10)
pnorm(1) - pnorm(-1)
pnorm(1, 0, 1) - pnorm(-1, 0, 1)


# Simulación para ver que sale algo parecido
xx <- rnorm(1000, 175, 10)
sum(between(xx, 165, 185)) / 1000

# Individuos entre mu-2*sigma y mu+2*sigma
pnorm(195, 175, 10) - pnorm(155, 175, 10)
sum(between(xx, 155, 195)) / 1000

# 3 sigma
pnorm(205, 175, 10) - pnorm(145, 175, 10)

# 4 sigma
pnorm(215, 175, 10) - pnorm(135, 175, 10)



##############################################################
# Aproximación de una binomial
##############################################################
# Probabilidad de ganar una  primitiva
1 / choose(49, 6)
# Número semanal medio de jugadores es 20e6
# Así que el número medio de acertantes es 
20e6 / choose(49, 6)

# Probabilidades de 1ue haya entre 0 y 15 acertantes una semana
dbinom(0:15, 20e6, 1/choose(49, 6))

n <- 20e6
p <- 1/choose(49, 6)
q <- 1 - p

# Aproximación de una binomial con una dist de Poisson
dbinom(0:15, n, p)
dpois(0:15, n * p)

# Aproximación de una binomial con una dist normal
dbinom(0:15, n, p)
pnorm(0:15+.5, n * p, sqrt(n * p * q)) - pnorm(0:15-.5, n * p, sqrt(n * p * q))

pnorm(1.5, n * p, sqrt(n * p * q)) - pnorm(0.5, n * p, sqrt(n * p * q))
dbinom(1, n, p)


##############################################################
# Aproximaciones de la función densidad
##############################################################
# Histograma de la altura de 10000 sujetos con precisión de 1 cm
xx1 <- round(rnorm(10000, 175, 10), 0)
table(xx1)
hist(xx1, seq(125, 225, by=1))

# Histograma de la altura de 10000 sujetos con precisión de 0.1 cm
xx2 <- round(rnorm(10000, 175, 10), 1)
table(xx2)
hist(xx2, seq(125, 225, by=.1))

# Histograma de la altura de 10000 sujetos con precisión de 0.01 cm
xx3 <- round(rnorm(10000, 175, 10), 2)
table(xx3)
hist(xx3, seq(125, 225, by=.01))

# Esto converge a una normal N(175, 10)
# Este valor = 0.04, me dice que hay aproximadamente un 4% de sujetos entre 174.5 y 175.5
# o un 0.4% entre 174.95 y 175.05
dnorm(175, 175, 10)

