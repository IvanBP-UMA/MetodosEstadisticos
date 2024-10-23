library(tidyverse)

dfMec <- read_csv("dfMec.csv")

M <- dfMec %>% transmute(x0= 1, x1=notaBach, x2=notaSel) %>% as.matrix()
Y <- matrix(dfMec$notaFinal)

solve(t(M) %*% M, t(M) %*% Y)
lm(notaFinal ~ notaBach + notaSel, dfMec)
