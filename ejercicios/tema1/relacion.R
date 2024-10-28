#################
# Ejercicio 13  #
#################

varY <- 4
meanY <- 8
m4Y <- 5648
CVY <- sqrt(varY)/meanY
m2Y <- varY + meanY^2

mu3 <- 0
m3Y <- 3*m2Y*meanY - 2*meanY^3

mu4 <- m4Y - 4*m3Y*meanY + 6*m2Y*meanY^2 - 3*meanY^4
g2 <- mu4/ varY^2 - 3

#################
# Ejercicio 19  #
#################

dat <- c(1.17, 1.61, 1.16, 1.38, 3.53, 1.23, 3.76, 1.94, 0.96, 4.75, 0.15, 2.41, 
         0.71, 0.02, 1.59, 0.19, 0.82, 0.47, 2.16, 2.01, 0.92, 0.75, 2.59, 3.07, 
         1.40)
plot(dat)


q1 <- quantile(dat, .25)
q3 <- quantile(dat, .75)
abline(c(q1, 0), col = 'red')
abline(c(q3, 0), col = 'red')

rangIQ <- q3 - q1
Ii <- q1 - 1.5*rangIQ
Is <- q3 + 1.5*rangIQ
Ei <- q1 - 3*rangIQ
Es <- q3 + 3*rangIQ

abline(c(Ii, 0), col = 'blue')
abline(c(Is, 0), col = 'blue')

dat[dat > Is]
dat[dat < Ii]

dat[dat > Es]
dat[dat < Ei]
