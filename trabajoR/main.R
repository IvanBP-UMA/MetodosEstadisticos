library('tidyverse')

# Ejercicio 1
# Leemos el fichero csv como tibble y nos hacemos que las variables cualitativas sean leídas como factores 
df <- read_csv('22797.csv', 
               col_types = cols(
                 .default = col_double(),
                 sexo = col_factor(),
                 dietaEsp = col_factor(),
                 nivEstPad = col_factor(),
                 nivEstudios = col_factor(),
                 nivIngresos = col_factor()
               ))

# Ejercicio 2
df <- df %>% mutate(IMC = peso/(altura)^2)

# Ejercicio 3
df <- df %>% drop_na()

# Ejercicio 4
# Con la funcion select nos quedamos con las columnas numericas del tibble y con map le aplicamos la funcion que sea necesaria
means <- df %>% select(where(is.double)) %>% map(mean)
# Creamos nuestra propia funcion para calcular la desviacion tipica pues la que está incluida en R calcula la cuasidesviacion
desvTip <- function(col) {
  sd(col) * sqrt((length(col)-1) / length(col))
}
desvs <- df %>% select(where(is.double)) %>% map(desvTip)

# 
