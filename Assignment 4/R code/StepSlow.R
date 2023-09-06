step.slow <- function(object, data, penalty=2, max.iter=50){
  ## object: A marima object
  ## data:   The same data as given as argument when fitting 'object'
  ## penalty: The penalty to be used
  ## max.iter: Number of iterations before evaluating the penalty
  # Init
  
  obj <- object # So that the original object is returned if no reduction is needed.
  
  ar.f <- object$ar.fvalues[,,-1]
  ma.f <- object$ma.fvalues[,,-1]
  ar.p <- object$out.ar.pattern[,,-1]
  ma.p <- object$out.ma.pattern[,,-1]
  ar.min <- min(ar.f[ar.f>0])
  ma.min <- min(ma.f[ma.f>0])
  # Now starting the actual model reduction
  while (min(ar.min, ma.min) < penalty){
    if (ar.min < ma.min){
      ar.p[ar.f ==ar.min] <- FALSE
    } else{
      ma.p[ma.f ==ma.min] <- FALSE
      
    } # else
    ## Now restimate
    obj <- marima(data, ar.pattern = check.one(ar.p), ma.pattern = check.one(ma.p), max.iter = max.iter)
    ar.f <- obj$ar.fvalues[,,-1]
    ma.f <- obj$ma.fvalues[,,-1]
    ar.p <- obj$out.ar.pattern[,,-1]
    ma.p <- obj$out.ma.pattern[,,-1]
    ar.min <- min(ar.f[ar.f>0])
    ma.min <- min(ma.f[ma.f>0])
  } # while
  return(obj)
}