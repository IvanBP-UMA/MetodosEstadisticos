library(tidyverse)

dfMec <- read_csv("dfMec.csv")

aux <- dfMec %>% select(notaFinal, notaBach)

covXY <- mean(aux$notaFinal*aux$notaBach) - mean(aux$notaFinal)*mean(aux$notaBach)
varX <- mean(aux$notaBach^2) - mean(aux$notaBach)^2
vary <- mean(aux$notaFinal^2) - mean(aux$notaFinal)^2

mod <- lm(notaFinal ~ notaBach, aux)
plot(dfMec$notaBach, dfMec$notaFinal)
abline(mod)

aux$notaPredicha <- predict.lm(mod)
aux %>% 
  mutate(notaPredicha = predict.lm(mod), error = (notaFinal - notaPredicha)) %>% 
  summarise(MSE = mean(error^2), R2=1 - MSE/(mean(notaFinal^2) - mean(notaFinal)^2))
