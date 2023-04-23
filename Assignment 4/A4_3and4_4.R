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
  if (!is.na(Y[tt,]) & (Y[tt,]<X.hat-6*sqrt(Sigma.yy) | Y[tt,]>X.hat+6*sqrt(Sigma.yy))){outliers = append(outliers,c(tt))}
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
    Sigma.xx.rec[tt] <- Sigma.xx # Each holding the sd for that point?
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
plot(Y, col = "red",pch = 20)
lines(out$pred)
matlines(out$pred[,1]+sqrt(Sigma.yy.pred[1,1,])%*%cbind(-1.96,1.96),col="seagreen",lty=c(2,2), lwd=1)

# Plot the standardized one step prediction errors
plot((Y-out$pred[1:5000])/(sqrt(Sigma.yy.pred[1,1,1:5000])),pch = 20)

### Now zoomin ###
# Plot the data with the 1 step prediction and prediction intervals 
plot(Y[800:950], col = "red",pch = 20) #,ylim = c(17,19)
lines(out$pred[800:950])
matlines(out$pred[800:950,1]+sqrt(Sigma.yy.pred[1,1,800:950])%*%cbind(-1.96,1.96),col="seagreen",lty=c(2,2), lwd=1)

# Plot the standardized one step prediction errors
plot((Y[800:950]-out$pred[800:950])/(sqrt(Sigma.yy.pred[1,1,800:950])),pch = 20)

     
### Report the values that defines the final state of the filter
# K = 0.7320508
# Sigma.xx.pred = 0.01366025
# Sigma.yy.pred = 0.01866025






