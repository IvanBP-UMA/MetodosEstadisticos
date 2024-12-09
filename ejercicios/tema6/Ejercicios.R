library('tidyverse')

#Ejericio 1
x <- 32000
s <- 4000
n <- 50

e <- qnorm(.975) * s/sqrt(n)
c(x -e, x + e)



#Ejercicio 11
data <- c(80, 70, 90, 75, 55, 80, 80, 65, 100, 75, 60, 60,
          75, 95, 80, 80, 90, 85, 70, 95, 75, 70, 85, 80,
          80, 65, 65, 50, 75, 75, 85, 85, 90, 70)
mean <- mean(data)
sd <- sd(data)
n <- length(data)

oo <- as.vector(table(data))
pi <- c(0, pnorm(seq(50, 100, by=5), mean, sd))
pi <- pi[-1] - pi[-12]
ei <- length(data) * pi
eC <- sum((oo - ei)^2 / ei)

1-pchisq(eC, 11 -2 -1)

#Ejercicio 12
oo <- matrix(c(6, 10, 10, 8, 12, 12, 8, 8, 14, 9, 14, 16), byrow=T, ncol=3)
rf <- rowSums(oo) / sum(oo)
cf <- colSums(oo) / sum(oo)

ee <- matrix(rep(cf, 4) * rep(rf, rep(3, 4)), byrow=T, ncol=3) * sum(oo)
eC <- sum ((oo - ee) ^ 2 / ee)
eC
qchisq(.95, 4)
qchisq(.05, 4)

# p-value
1 - pchisq(eC, 4)


