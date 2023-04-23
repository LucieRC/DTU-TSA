## Assignment 4 v2

data <- read.csv("A4_Kulhuse.csv") ; head(data)

A <- matrix(c(1),nrow=1) ; A
B <- matrix(c(0),nrow=1) ; B
C <- matrix(c(1),nrow=1) ; C
Sigma1 <- matrix(c(0.01)) ; Sigma1
Sigma2 <- matrix(0.005) ; Sigma2
Y <- data$Sal ; head(Y)


# Sourcing my implementation of the Kalman filter...
source("../11/kalman.R")
# With good starting guesses
k1 <- kalman(Y=Y, A=A, B=B, C=C,
             Sigma.1=Sigma1, Sigma.2=Sigma2, Xhat0=0, verbose=TRUE) #u=matrix(0),
names(k1) #attributes(k1)
sd <- sqrt(k1$Sigma.xx.rec[1,1,])
matlines(k1$rec[,1] + 1.96*cbind(0,c(0,sd), c(0,-sd)), col=2, lty=c(1,2,2))

plot(sd)

# Wrong initial state:
dev.off()
k1 <- kalman(Y=Y, A=A, B=B, u=matrix(rep(g,length(Y))), C=C,
             Sigma.1=Sigma1*100, Sigma.2=Sigma2, Xhat0=c(6000,-100), verbose=TRUE)
sd <- sqrt(k1$Sigma.xx.rec[1,1,])
plot(Y)
matlines(k1$rec[,1] + 1.96*cbind(0,c(0,sd), c(0,-sd)),col=3,lty=c(1,2,2))
matlines(sd)

# Final state:
attributes(k1)
tail(k1$pred)
