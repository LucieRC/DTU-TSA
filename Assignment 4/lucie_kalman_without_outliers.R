kalman <- function(Y,A,B=NULL,u=NULL,C,Sigma.1=NULL,Sigma.2=NULL,debug=FALSE,V0=Sigma.1,Xhat0=NULL,n.ahead=1,skip=0,verbose=FALSE){
  
  ## predictions through data are one-step predictions. n.ahead means
  ## how long we must keep predict after data. These are of course
  ## predictions of different step lengths.
  
  ## Y has to be columns. 
  if(class(Y)=="numeric"){
    dim.Y <- c(length(Y),1)
    Y <- matrix(Y,ncol=1)
  } ## else {
  dim.Y <- dim(Y)
  ##  }
  
  ## Definition of default variables
  ## A and C must be supplied
  nstates <- dim(A)[1]
  
  ## these default values don't make much sense
  if(is.null(Sigma.1)){
    Sigma.1 <- diag(rep(1,nstates))
  }
  if(is.null(Sigma.2)){
    Sigma.2 <- diag(rep(1,dim.Y[2]))
  }
  
  if(is.null(B)){
    B <- matrix(rep(0,nstates),ncol=1)
  }
  if(is.null(u)){
    u <- matrix(rep(0,dim.Y[1]+n.ahead),ncol=1)
  }
  if(is.null(V0)){
    V0 <- Sigma.1
  }
  if(is.null(Xhat0)){
    Xhat0 <- matrix(rep(0,nstates),ncol=1)
  }
  
  
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
  
  
  for(tt in ( +1):dim.Y[1]){
    #print(tt)
    ## (10.75) (8.75)
    K <- Sigma.xx%*%t(C)%*%solve(Sigma.yy)
    
    ## (10.73) (8.73) - reconstruction
    if(!any(is.na(Y[tt,])) & Y[tt,]){ # At first everything is thrown away if one is missing
      X.hat <- X.hat+K%*%(t(Y[tt,])-C %*% as.matrix(X.hat))
      X.rec[tt,] <- X.hat
      ## (10.74) (8.74)
      Sigma.xx <- Sigma.xx-K%*%C%*%Sigma.xx
    }
    
    if(verbose){
      Sigma.xx.rec[,,tt] <- Sigma.xx
      Sigma.yy.rec[,,tt] <- Sigma.yy
    }
    
    ##(10.76) (8.76) - prediction
    X.hat <- A%*%X.hat + B%*%t(matrix(as.numeric(u[tt,]),nrow=1))
    X.pred[tt+1,] <- X.hat
    
    ##(10.77) (8.77)
    Sigma.xx <- A%*%Sigma.xx%*%t(A)+Sigma.1
    ##(10.78) (8.78)
    Sigma.yy <- C%*%Sigma.xx%*%t(C)+Sigma.2
    
    if(verbose){
      K.out[,,tt] <- K
      #### these are the prediction error variance-covariances
      Sigma.xx.pred[,,tt+1] <- Sigma.xx
      Sigma.yy.pred[,,tt+1] <- Sigma.yy
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
  return(out)
}

