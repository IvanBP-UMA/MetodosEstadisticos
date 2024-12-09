##############################################################
# Métodos estadísticos para la computación
# Escuela Técnica Superior de Ingeniería Informática.
# Universidad de Málaga. Curso 2020 / 21
# Tema 4. Probabilidad
##############################################################
library(tidyverse)


##############################################################
# Funciones para calcular conjuntos combinatorios
# En pocas líneas definimos 5 funciones combinatorias
# variaciones, Combinaciones, Variaciones con repetición, 
#   Combinaciones con repetición y Permutaciones
##############################################################
combU <- function(f, elems, n){
  if (!n) list(integer(0)) else
  if (!length(elems)) list() else
    elems %>% imap(~map(combU(f, f(elems, .y), n - 1), c, .x)) %>% reduce(c) 
}

vari  <- partial(combU, function(e, i) e[-i])
comb  <- partial(combU, function(e, i) e[-1:-i])
variR <- partial(combU, function(e, i) e)
combR <- partial(combU, function(e, i) e[i:length(e)])
permu <- function(e) vari(e, length(e))

##############################################################
# Si quieres entender como funciona combU, aquí tienes como sería 
#   definir dos de las funciones directamente
##############################################################
# variR <- function(elems, n){
#   if (!n) list(integer(0)) else
#   if (!length(elems)) list() else
#     elems %>% map(~map(variR(elems, n - 1), c, .x)) %>% reduce(c) 
# }
# comb <- function(elems, n){
#   if (!n) list(integer(0)) else
#   if (!length(elems)) list() else
#     elems %>% imap(~map(comb(elems[-1:-.y], n - 1), c, .x)) %>% reduce(c) 
# }


##############################################################
# Ejemplos de manejar conjuntos (sucesos) en R
##############################################################
A <- vari(letters[1:5], 2)
B <- vari(letters[3:7], 2)

union(A, B)
intersect(A, B)
setdiff(A, B)
setequal(A, B)


##############################################################
# Ejemplos de cálculo de cantidades combinatorias
##############################################################
# Combinaciones de 10 elementos tomados de 3 en 3
choose(10, 3)

# Combinaciones de 10 elementos tomados de 3 en 3 con Repetición
choose(10 + 3 - 1, 3)

# Variaciones de 10 elementos tomados de 3 en 3
factorial(3) * choose(10, 3)

# Variaciones de 10 elementos tomados de 3 en 3 con Repetición
10 ^ 3

# Para números muy grandes, son interesantes lchoose y lfactorial
# Calcula logaritmos de las cantidades
lfactorial(1000)
lchoose(10000, 5000)


##############################################################
# Ejemplo: Generar una baraja española y extraer dos cartas ordenadas
##############################################################
cartas <- map(c("O", "C", "B", "E"),str_c, 1:10) %>% reduce(c)
vari(cartas, 2)


##############################################################
# Ejemplo: Una urna con 7 bolas blancas y 5 rojas
##############################################################
urna <- map2(c("B", "R"), list(1:7, 1:5), str_c) %>% reduce(c)
# Extraemos dos bolas al mismo tiempo
extr <- comb(urna, 2)
# Nos quedamos con todas las extraciones dónde las dos bolas son blancas
extB <- extr %>% keep(~all(startsWith(., "B")))
# Nos quedamos con todas las extraciones dónde las dos bolas son rojas
extR <- extr %>% keep(~all(startsWith(., "R")))
# Extracciones con las dos bolas son iguales
extEq <- union(extB, extR)
# Como las extracciones son equiprobables, podemos calcular la probabilidad de extraer dos bolas iguales
#  dividiendo casos favorables por vasos posibles.
length(extEq) / length(extr)

