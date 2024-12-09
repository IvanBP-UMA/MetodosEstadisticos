##############################################################
# M?todos estad?sticos para la computaci?n
# Escuela T?cnica Superior de Ingenier?a Inform?tica.
# Universidad de M?laga. Curso 2020 / 21
# Tema 5a. Variable aleatoria
##############################################################
library(tidyverse)


##############################################################
# Funci?n densidad de ejemplo
##############################################################
# Esto es la funci?n de densidad del ejemplo de las diapositivas
# Esta funci?n no es vectorial. No se le puede pasar un vector y que lo evalue de golpe.
# Como vamos a utilizar integrate necesitamos que la funci?n densidad que usemos sea vectorial.
# OJO: Tal cual est? NO nos vale.
densV0 <- function(x) {
  if (x < 1)      0
  else if (x < 2) x -1
  else if (x < 3) 3 - x
  else 0
}

# Con map podemos convertir cualquier funci?n de n?meros at?micos en una funci?n vectorial.
# Esta funci?n ya s? nos valdr?a.
densVv <- function(x) map_dbl(x, densV0)

# Pero es m?s eficiente hacerlo directamente utilizando la funci?n vectorial ifelse
# As? que utilizaremos esta versi?n.
# Es equivalente a la anterior densVv, pero usamos esta porque es m?s eficiente.
densV1 <- function(x) {
  ifelse(x < 1, 0, 
  ifelse(x < 2, x -1, 
  ifelse(x < 3, 3 - x, 
                0)))
}



##############################################################
# Comprobaci?n de que su integral vale 1
##############################################################
# Esto tiene que dar 1
integrate(densV1, -Inf, Inf)



##############################################################
# Obtenci?n num?rica de la funci?n de distribuci?n
##############################################################
# OJO: Esto es s?lo una estimaci?n num?rica.
# OJO: Hay que saber calcular la funci?n de distribuci?n de manera algebr?ica.
# Esta funci?n recibe como argumento la funci?n de densidad y un valor x
# y devuelve el valor de la funci?n de distribuci?n F(x)
distV <- function(densV, x) {
  integrate(densV, -Inf, x)$value
}

# Esto es m?s "estilo funcional"
# Recibe una funci?n densidad y devuelve una funci?n de distribuci?n.
# Ojo: as? definida no es una funci?n vectorial
distF <- function(densV) {
  function(x) integrate(densV, -Inf, x)$value
}

# Ejemplo de uso.
distV(densV1, 2)
distV(densV1, 2.5)

# Metemos en distF1 la nueva funci?n
distF1 <- distF(densV1)
# La usamos todas las veces que queramos
distF1(2)
distF1(2.5)



##############################################################
# C?lculo de las probabilidades que se piden en el ejemplo
##############################################################
# OJO: Es una estimaci?n num?rica. Hay peque?as variaciones de precisi?n
# Directamente con la funci?n de densidad
integrate(densV1, -Inf, 1.5)
integrate(densV1, 2.3, Inf)
integrate(densV1, 1.1, 1.7)
integrate(densV1, 1.5, 2.5)

# Lo mismo con la funci?n de distribuci?n
distF1(1.5)
1 - distF1(2.3)
distF1(1.7) - distF1(1.1)
distF1(2.5) - distF1(1.5)



##############################################################
# Esperanza matem?tica
##############################################################
# OJO: Es una estimaci?n num?rica. Hay peque?as variaciones de precisi?n
# Funciones de esperanza matem?tica discreta y continua
espMD <- function(x, p)  sum(x * p)
espMC <- function(densV) integrate(function(x) x * densV(x), -Inf, Inf)$value
 
# Ejemplo: lanzamiento de un dado
x <- 1:6
p <- rep(1/6, 6)
espMD(x, p)

# Esperanza del ejemplo densV1
espMC(densV1)



##############################################################
# Momentos ordinarios y centrales. Caso discreto y continuo
##############################################################
# OJO: Es una estimaci?n num?rica. Hay peque?as variaciones de precisi?n
# Variables discretas
mOrdD  <- function(x, p, n)  sum(x ^ n * p)
mCentD <- function(x, p, n)  sum((x - espMD(x, p)) ^ n * p)

# Variables continuas
mOrdC  <- function(densV, n) integrate(function(x) x ^ n * densV(x), -Inf, Inf)$value
mCentC <- function(densV, n) integrate(function(x) (x - espMC(densV)) ^ n * densV(x), -Inf, Inf)$value

# Ejemplo: C?lculo de la varianza discreta
# De las dos manera habituales, para ver que coinciden
mCentD(x, p, 2)
mOrdD(x, p, 2) - espMD(x, p) ^ 2

# Ejemplo: C?lculo de la varianza continua
# De las dos manera habituales, para ver que coinciden
mCentC(densV1, 2)
mOrdC(densV1, 2) - espMC(densV1) ^ 2



##############################################################
# C?lculo de cuantiles
##############################################################
# OJO: Es una estimaci?n num?rica. Hay peque?as variaciones de precisi?n
# Funci?n para calcular el cuantil de una variable continua
# Hay que pasarle un intervalo d?nde se estima que est? el resultado.
cuantilC <- function(densV, c, interv) {
  uniroot(function (x) distV(densV, x) - c, interv)$root
}

# Mediana
cuantilC(densV1, .5, c(0, 5))

# Percentil 10
cuantilC(densV1, .1, c(0, 5))




##############################################################
# Ejercicio 5.1
##############################################################
# Apartado a
# Primero hay que calcular manualmente que a vale 1/3
# Alternativamente, podr?amos hacerlo en R, definiendo una funci?n que dependa del par?metro:
densParam <- function(a, x) {
  ifelse(x < -1, 0, 
  ifelse(x <  0, 2 * a * (x+1), 
  ifelse(x <  2, a * (2 - x), 
                 0)))
}
# Y otra que diga para cada par?metro cuanto se desvia la integral de 1
funcionObj <- function(a) integrate(function(x) densParam(a, x), -Inf, Inf)$value - 1
# Y ahora buscamos para qu? par?metro esa desviaci?n es 0
uniroot(funcionObj, c(-10, 10))

# Una vez que tenemos claro que el par?metro es 1/3:
densV51 <- function(x) {
  ifelse(x < -1, 0, 
  ifelse(x <  0, 2/3 * (x+1), 
  ifelse(x <  2, 1/3 * (2 - x), 
                 0)))
}

# Apartado b
# La funci?n de distribuci?n habr?a que calcularla anal?ticamente.
# Funci?n de distribuci?n num?rica:
distF51 <- distF(densV51)
# Representaci?n gr?fica
x <- seq(-2, 3, length.out = 101)
# Ojo, distf51 no es una funci?n vectorial, as? que necesitamos map para aplicarla a un vector
y <- map_dbl(x, distF51)
plot(x, y, type="l")

# Apartado c
# Esperanza matem?tica:
espMC(densV51)
# Mediana:
cuantilC(densV51, .5, c(-1, 2))
# Moda
x[which.max(densV51(x))]

# Apartado d
# Varianza:
var51 <- mCentC(densV51, 2)
var51

# Apartado e
# Coeficiente de asimetr?a
mCentC(densV51, 3) / var51^(3/2)
# Curtosis
mCentC(densV51, 4) / var51^2

# Apartado g
# P(X < 1)
distF51(1)
# P(X > -0.5)
1 - distF51(-.5)
# P(|X| < 0.3)
distF51(.3) - distF51(-.3)
