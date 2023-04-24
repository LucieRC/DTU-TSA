library("FKF")
# Load data

data = read.csv('Time Series Analysis/Assignment 4/A4_Kulhuse.csv',sep=",",header=T)

# initial visualization
#plot(data$Sal,type = "l",ylim=c(17,21))

# There are at least a few outliers

#N = 1200
#Y <- data$Sal[1:N]

A = matrix(1)
C = matrix(1)

# Define data in a matrix with each variable in a different columns
Y <- matrix(data$Sal,ncol=1)

### Runes filter ###
Sigma.2 = 0.005
Sigma.1 = 0.01
V0=Sigma.1
Xhat0=Y[1]
skip=0
n.ahead=1
verbose=TRUE

## predictions through data are one-step predictions. n.ahead means
## how long we must keep predict after data. These are of course
## predictions of different step lengths.

## Y has to be columns. 
#if(class(Y)=="numeric"){
#  dim.Y <- c(length(Y),1)
#  Y <- matrix(Y,ncol=1)
#} ## else {

dim.Y <- dim(Y)
##  }

## Definition of default variables
## A and C must be supplied
nstates <- dim(A)[1]

## i stedet for (10.79)
X.hat <- Xhat0
## (10.80)
Sigma.xx <- V0
## (10.78) (8.78)
Sigma.yy <- C%*%Sigma.xx%*%t(C)+Sigma.2

## for saving reconstruction
X.rec <- array(dim=c(dim.Y[1]+n.ahead,nstates))
X.pred <- array(dim=c(dim.Y[1]+n.ahead,nstates))

## for saving K, Sigmas.
if(verbose){
  K.out <- array(dim=c(dim(Sigma.xx%*%t(C)%*%solve(Sigma.yy)),dim.Y[1]))
  Sigma.xx.rec <- array(dim=c(dim(Sigma.xx),dim.Y[1]))
  Sigma.yy.rec <- array(dim=c(dim(Sigma.yy),dim.Y[1]))   
  Sigma.xx.pred <- array(dim=c(dim(Sigma.xx),dim.Y[1]+n.ahead))
  Sigma.yy.pred <- array(dim=c(dim(Sigma.yy),dim.Y[1]+n.ahead))
  
}

outliers = c()

for(tt in (1):dim.Y[1]){
  ## (10.75) (8.75)
  K <- Sigma.xx%*%t(C)%*%solve(Sigma.yy)
  
  # 4.4: remove outliers
  # Make a list of outlier indexes
  #if (!is.na(Y[tt,]) & (Y[tt,]<X.hat-6*sqrt(Sigma.yy) | Y[tt,]>X.hat+6*sqrt(Sigma.yy))){outliers = append(outliers,c(tt))}
  #if (is.na(Y[tt,]) | Y[tt,]<X.hat-6*sqrt(Sigma.yy)){Y[tt,]=NA}
  #if (is.na(Y[tt,]) | Y[tt,]>X.hat+6*sqrt(Sigma.yy)){Y[tt,]=NA}
  
  ## (10.73) (8.73) - reconstruction
  if(!any(is.na(Y[tt,]))){ # At first everything is thrown away if one is missing
    X.hat <- X.hat+K%*%(t(Y[tt,])-C %*% as.matrix(X.hat))
    X.rec[tt,] <- X.hat
    ## (10.74) (8.74)
    Sigma.xx <- Sigma.xx-K%*%C%*%Sigma.xx
  }
  
  if(verbose){
    Sigma.xx.rec[tt] <- Sigma.xx
    Sigma.yy.rec[tt] <- Sigma.yy
    #Sigma.xx.rec[,,tt] <- Sigma.xx
    #Sigma.yy.rec[,,tt] <- Sigma.yy
  }
  
  ##(10.76) (8.76) - prediction
  X.hat <- A%*%X.hat #+ B%*%t(matrix(as.numeric(u[tt,]),nrow=1))
  X.pred[tt+1,] <- X.hat
  
  ##(10.77) (8.77)
  Sigma.xx <- A%*%Sigma.xx%*%t(A)+Sigma.1
  ##(10.78) (8.78)
  Sigma.yy <- C%*%Sigma.xx%*%t(C)+Sigma.2
  
  if(verbose){
    K.out[,,tt] <- K
    Sigma.xx.pred[tt+1] <- Sigma.xx
    Sigma.yy.pred[tt+1] <- Sigma.yy
    #### these are the prediction error variance-covariances
    #Sigma.xx.pred[,,tt+1] <- Sigma.xx
    #Sigma.yy.pred[,,tt+1] <- Sigma.yy
  }
  
}

if(n.ahead>1){
  for(tt in dim.Y[1]+(1:(n.ahead-1))){
    X.hat <- A%*%X.hat + B%*%t(matrix(u[tt,],nrow=1))
    X.pred[tt+1,] <- X.hat
    Sigma.xx <- A%*%Sigma.xx%*%t(A)+Sigma.1
    Sigma.xx.pred[,,tt+1] <- Sigma.xx
    Sigma.yy.pred[,,tt+1] <- C%*%Sigma.xx%*%t(C)+Sigma.2
  }
}
if(verbose){
  out <- list(rec=X.rec,pred=X.pred,K=K.out,Sigma.xx.rec=Sigma.xx.rec,Sigma.yy.rec=Sigma.yy.rec,Sigma.xx.pred=Sigma.xx.pred,Sigma.yy.pred=Sigma.yy.pred)
} else {
  out <- list(rec=X.rec,pred=X.pred)
}

# Plot the data with the 1 step prediction and prediction intervals 
plot(Y,pch='.',col = "black")
lines(out$pred,col="red")
matlines(out$pred[,1]+sqrt(Sigma.yy.pred[1,1,])%*%cbind(-1.96,1.96),col="red",lty=c(2,2), lwd=1)

# Plot the standardized one step prediction errors
plot((Y-out$pred[1:5000])/(sqrt(Sigma.yy.pred[1,1,1:5000])),pch = 20,xlab = "Time",ylab = "Residuals")

### Now zoomin ###
# Plot the data with the 1 step prediction and prediction intervals 
plot(Y[800:950],col="black",pch = 16,ylim = c(15,18.7),xlab = "Time",ylab = "Salinity") # #,ylim = c(16.9,18.6)
lines(out$pred[800:950], col = "red", lwd = 2)
matlines(out$pred[800:950,1]+sqrt(Sigma.yy.pred[1,1,800:950])%*%cbind(-1.96,1.96),col="red",lty=c(2,2), lwd=2)

# Plot the standardized one step prediction errors
plot((Y[800:950]-out$pred[800:950])/(sqrt(Sigma.yy.pred[1,1,800:950])),pch = 20)

######################################################################################################
############################################## 4.5 ###################################################
######################################################################################################
Y <- matrix(data$Sal[1:800],ncol=1)

source("Time Series Analysis/Day 11/kalman.R")

par = c(0.01,0.005)

my.obj <- function(par){
  Kro <- kalman(Y, A= A, C=C, Sigma.1=par[1], Sigma.2=par[2],
                V0=Sigma.1, Xhat0=Y[1],n.ahead=1,verbose=TRUE)
  nepso <- (Y[-1]-Kro$pred[-c(1, 800+1),1])^2 / Kro$Sigma.yy.pred[-c(1, 800+1)]
  return(0.5 * sum(nepso + log(Kro$Sigma.yy.pred[-c(1, 800+1)]))) # Maximum likelihood
}

par = c(10,10)
my.obj(par)

(Kmopt <- optim(c(0.01,0.01), my.obj, method = "L-BFGS-B", lower=c(0.0001,0.00000001)))

Kmopt$par

# In my implementation the observation variance keeps converging to the lower bound, 
# no matter what I set it to. I cant set it to zero, because the log(0)=-Inf. The lower 
# bound for observation variance also very much dictates what Sigma_1 becomes. 

Y <- matrix(data$Sal[800:5000],ncol=1)
K2 <- kalman(Y, A= A, C=C, Sigma.1=Kmopt$par[1], Sigma.2=Kmopt$par[2],
              V0=Sigma.1, Xhat0=Y[1],n.ahead=1,verbose=TRUE)

### Now zoomin ###
# Plot the data with the 1 step prediction and prediction intervals 
plot(Y[1:150],col="black",pch = 16,ylim = c(15,18.7),xlab = "Time",ylab = "Salinity") # #,ylim = c(16.9,18.6)
lines(K2$pred[1:150], col = "red", lwd = 2)
matlines(K2$pred[1:150]+sqrt(K2$Sigma.yy.pred[1,1,1:150])%*%cbind(-1.96,1.96),col="red",lty=c(2,2), lwd=2)

# Values that defines the final state of the filter
K2$pred[4202]



######### Runes code for optimizing parameters #############


library(MASS)
## Initialising
z0 <- 10000 # start at a hight of 10000 meters
A <- matrix(c(1,0,1,1),nrow=2)
B <- matrix(c(-.5,-1),nrow=2)
C <- matrix(c(1,0),nrow=1)
Sigma1 <- matrix(c(2,.8,.8,1),nrow=2) #*1000
Sigma2 <- matrix(10000)
g <- 9.82; 
N <- 100
X <- matrix(nrow=2,ncol=N)
X[,1] <- c(z0,0)
Y <- numeric(N)
Y[1] <- C%*%X[,1]+sqrt(Sigma2) %*% rnorm(1)

## Simulating
for (I in 2:N){
  X[,I] <- A %*% X[,I-1,drop=FALSE] + B%*%g +
    mvrnorm(mu=rbind(0,0),Sigma=Sigma1)
  #chol(Sigma1) %*% matrix(rnorm(2),ncol=1)
  Y[I] <- C %*% X[,I] + sqrt(Sigma2) %*% rnorm(1)
}
Nhit <- min(which(X[1,]<0))-1
X <- X[,1:Nhit]
Y <- Y[1:Nhit]

par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
matplot(cbind(X[1,],Y),type="o",pch=c(NA,20),col=c("Steelblue","tomato"),xlab="Time (s)", ylab="Altitude (m)")
legend("topright",legend=c("True altitude","Observations"),col=c("steelblue","tomato"),pch=c(NA,20),bty='n',lty=c(1,NA))
#plot(X[2,])

## Trying to optimize ...
## Objective function

my.obj <- function(par){
  Kro <- kalman(Y, A= A, B=B, u=matrix(par[2],length(Y),1), C=C, Sigma.1=Sigma1, Sigma.2=Sigma2,
                V0=diag(c(100,0)), Xhat0=rbind(par[1],0),n.ahead=1,verbose=TRUE)
  nepso <- (Y[-1]-Kro$pred[-c(1, Nhit+1),1])^2 / Kro$Sigma.yy.pred[1,1,-c(1, Nhit+1)]
  return(0.5 * sum(nepso + log(Kro$Sigma.yy.pred[1,1,-c(1, Nhit+1)])))
}

(Kmopt <- optim(c(9000,-8), my.obj, method = "L-BFGS-B"))

Kmopt$par




