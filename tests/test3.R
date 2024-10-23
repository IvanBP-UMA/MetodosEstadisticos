library(tidyverse)

dfMec <- read_csv("dfMec.csv",
                  col_types = cols(
                    .default = col_double(),
                    sexo = col_factor(),
                    nivEstPad = col_factor(),
                    academia = col_factor(),
                    emailMovil = col_factor()
                  ))
spec(dfMec)

mod <- lm(notaFinal ~ notaBach:nivEstPad, dfMec)
mod
summary(mod)$r.squared
