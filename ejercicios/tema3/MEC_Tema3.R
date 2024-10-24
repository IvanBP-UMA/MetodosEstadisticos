##############################################################
# M�todos estad�sticos para la computaci�n
# Escuela T�cnica Superior de Ingenier�a Inform�tica.
# Universidad de M�laga. Curso 2020 / 21
# Tema 3. Series temporales
##############################################################
library('tidyverse')

# La carpeta d�nde tengas los datos
localFolder='c:/data/'

# leemos los datos
# Le indicamos la frecuencia de la componente estacional
xx <- scan('ts01.dat')
uk <- ts(xx, start=c(1969, 1), frequency=4)

# Ejemplo del primer valor de la media movil de orden 3.
(uk[1] * .5 + uk[2] + uk[3] + uk[4] + uk[5] * .5) / 4

# Componente tendencia, mediante una media movil de orden 3
tend <- stats::filter(uk, c(.5, 1, 1, 1, .5) / 4)
# Gr�fico de la serie con la tendencia en rojo
plot(uk)
lines(tend, col=2)


##############################################################
# Descomposici�n multiplicativa
##############################################################
# Componente estacional * aleatoria
est_aleM <- uk / tend
# Componente estacional sin normalizar
est1M <- colMeans(matrix(est_aleM, ncol=4, byrow=T), na.rm=T)
# Normalizo la componente estacional
estM  <- est1M / mean(est1M)
# Componente estacional como serie temporal
estMC <- ts(rep(estM, 20), start=c(1969, 1), frequency=4)
# Componente aleatoria
aleM  <- est_aleM / estM
# Serie desestacionalizada
desestM <- uk / estM
# Gr�fico de la serie desestacionalizada
plot(uk)
lines(desestM, col=4)


##############################################################
# Descomposici�n aditiva
##############################################################
# Componente estacional * aleatoria
est_aleA <- uk - tend
# Componente estacional sin normalizar
est1A <- colMeans(matrix(est_aleA, ncol=4, byrow=T), na.rm=T)
# Normalizo la componente estacional
estA  <- est1A - mean(est1A)
# Componente estacional como serie temporal
estAC <- ts(rep(estA, 20), start=c(1969, 1), frequency=4)
# Componente aleatoria
aleA  <- est_aleA - estA
# Serie desestacionalizada
desestA <- uk - estA
# Gr�fico de la serie desestacionalizada
plot(uk)
lines(desestA, col=4)


##############################################################
# Calculamos una regresi�n lineal de la serie desestacionalizada
##############################################################
# Esto mete en xx un vector 1:n, donde n es el tama�o de uk
xx <- seq_along(uk)
# regresi�n lineal
model <- lm(desestM ~ xx)
# Calculamos los valores predichos por la recta
regL <- ts(predict.lm(model), start=c(1969, 1), frequency=4)
# Hacemos un gr�fico de la serie desestacionalizada con la recta
plot(desestM)
lines(regL, col=2)


##############################################################
# Uso del comando decompose. Debemos obtener los mismos resultados.
##############################################################
descM <- decompose(uk, type="mul")
descA <- decompose(uk, type="add")
plot(descM)


##############################################################
# Autocorrelaci�n
##############################################################
# Funci�n que calcula la autocorrelaci�n para una frecuencia n
autoCorrel <- function(data, n) {
  # Quita los n �ltimos datos
  xx1 <- head(data, -n)   
  # Quita los n primeros datos
  xx2 <- tail(data, -n)
  # correlaci�n
  summary(lm(xx1 ~ xx2))$r.squared
}

# Calculamos la autocorrelaci�n para frecuencia 4
autoCorrel(uk, 4)


# Calculamos las autocorrelaciones para frecuencias 1 a 12
acValues <- map_dbl(1:12, autoCorrel, data=uk)
# Y vemos que el m�ximo se alcanza con autocorrelaci�n 4, as� que es la frecuencia correcta.
acValues
which.max(acValues)
