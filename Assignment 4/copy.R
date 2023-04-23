## Week 11

setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/10")
## falling body example.
## We start at 10000 m and drop the body.
z0 <- 10000
A <- matrix(c(1,0,1,1),nrow=2)
B <- matrix(c(-.5,-1),nrow=2)
C <- matrix(c(1,0),nrow=1)
Sigma1 <- 100*matrix(c(2,.8,.8,1),nrow=2) ; Sigma1
Sigma2 <- matrix(10000)

g <- 9.82 ## m/s2
N <- 300
X <- matrix(nrow=2,ncol=N)
X[,1] <- c(z0,0)
Y <- numeric(N)
Y[1] <- C%*%X[,1]+sqrt(Sigma2) %*% rnorm(1)
## Simulation
for (I in 2:N){
  X[,I] <- A%*%X[,I-1,drop=FALSE]+B%*%g+chol(Sigma1) %*% matrix(rnorm(2),ncol=1)
  Y[I] <- C%*%X[,I]+sqrt(Sigma2) %*% rnorm(1)
}

Nhit <- min(which(X[1,]<0))-1
X <- X[,1:Nhit]
Y <- Y[1:Nhit]

plot(X[1,])
plot(X[2,])
plot(Y)


# Sourcing my implementation of the Kalman filter...
source("../11/kalman.R")
# With good starting guesses
k1 <- kalman(Y=Y, A=A, B=B, u=matrix(rep(g,length(Y))), C=C,
             Sigma.1=Sigma1, Sigma.2=Sigma2, Xhat0=X[,1], verbose=TRUE)
names(k1) #attributes(k1)
sd <- sqrt(k1$Sigma.xx.rec[1,1,])
matlines(k1$rec[,1] + 1.96*cbind(0,c(0,sd), c(0,-sd)), col=2, lty=c(1,2,2))

plot(sd)

# Wrong initial state:
k1 <- kalman(Y=Y, A=A, B=B, u=matrix(rep(g,length(Y))), C=C,
             Sigma.1=Sigma1*100, Sigma.2=Sigma2, Xhat0=c(6000,-100), verbose=TRUE)
sd <- sqrt(k1$Sigma.xx.rec[1,1,])
plot(Y)
matlines(k1$rec[,1] + 1.96*cbind(0,c(0,sd), c(0,-sd)),col=3,lty=c(1,2,2))

sd <- sqrt(k1$Sigma.xx.pred[1,1,])
matlines(k1$pred[,1] + 1.96*cbind(0,c(sd), c(-sd)),col=4,lty=c(1,2,2))


## Plotting the velocity
sd <- sqrt(k1$Sigma.xx.rec[2,2,])
matplot(k1$rec[,2] + 1.96*cbind(0,c(0,sd), c(0,-sd)),col=3,lty=c(1,2,2), type="l")

