library("forecast")
source("step.slow.marima.2017")
library(marima)

A3Data_full <- read.csv("A3Data.csv", head=T)
A3Data <- A3Data_full[1:122,]

# chose columns to investigate
selects <- c(2,3,4,5,6)
HousePrices <- data.frame(A3Data[,selects])
colnames(HousePrices) <- colnames(A3Data)[selects]
kvar <- length(selects)
k <- c(1:kvar)


# log transform
log.HousePrices <- log(HousePrices)

# difference data

difs <- rep(1, kvar)
difference<-c(1, 1, 2, 1,3,1) 

dlog.HousePrices <- t(define.dif(log.HousePrices,difference)$y.dif)

# acf & pacf
acf(dlog.HousePrices)
pacf(dlog.HousePrices)

#
#ar(1,2), ma(2,4,8,12)

ar <-c(1)
ma <-c(2,4,8)

mod <- define.model(kvar = kvar, ar=ar, ma=ma)
Model <- marima(dlog.HousePrices, means = 1, ar.pattern=mod$ar.pattern, ma.pattern=mod$ma.pattern,
                Check=FALSE, Plot = "trace", penalty=0)

slp <- step.slow.p(Model, dlog.HousePrices, p.value = 0.05)

Forecasts <-  arma.forecast(t(rbind(ts(dlog.HousePrices), matrix(NA,6,kvar))), nstart=(dim(dlog.HousePrices)[1]), nstep=6, marima=slp)


plot(exp(diffinv(dlog.HousePrices[,1]))*1000)
lines(exp(diffinv(Forecasts$forecasts[1,]))*1000, col='steelblue')
legend("topleft",c("Data", "Predicted"),col=c("black","steelblue"),lty=1,bty='n')














# possible model ARIMA
mod <- define.model(kvar = kvar, ar=c(1),ma=c(2,6,12,18))
arp<-mod$ar.pattern
map<-mod$ma.pattern

short.form(arp)
short.form(map)


Model <- marima(dlog.HousePrices,ar.pattern=arp, ma.pattern=map,penalty=2.0)

ar.model <- Model$ar.estimates
ma.model <- Model$ma.estimates

dif.poly <- define.dif(log.HousePrices,difference)$dif.poly  # = difference polynomial in ar-form.
# Multiply the estimated ar-polynomial with difference polynomial
# to compute the aggregated ar-part of the arma model:
#

ar.aggregated <- pol.mul(ar.model, dif.poly, L=12)
# and print everything out in 'short form':
#
short.form(ar.aggregated, leading=FALSE)
short.form(ma.model, leading=FALSE)






step.slow <- function(object, data, penalty=2, max.iter=50, verbose=FALSE){
  ## object: A marima object
  ## data:   The same data as given as argument when fitting 'object'
  ## penalty: The penalty to be used
  ## max.iter: Number of iterations before evaluating the penalty
  ## verbose: Extra printing so that one can see which terms ar left out in each iteration
  
  # Init
  
  obj <- object # So that the original object is returned if no reduction is needed.
  "[" <- function(x, ...) .Primitive("[")(x, ..., drop = FALSE)
  
  if (any(obj$ar.fvalues[,,-1] >0)){
    ar.f <- obj$ar.fvalues[,,-1]
    ar.p <- obj$out.ar.pattern[,,-1]
    ar.min <- min(ar.f[ar.f>0])
  } else{
    ar.min <- Inf
    ar.p <- obj$out.ar.pattern
  }
  if (any(obj$ma.fvalues[,,-1] >0)){
    ma.f <- obj$ma.fvalues[,,-1]
    ma.p <- obj$out.ma.pattern[,,-1]
    ma.min <- min(ma.f[ma.f>0])
  } else {
    ma.min <- Inf
    ma.p <- obj$out.ma.pattern
  }
  print(c(ar.min, ma.min))
  # Now starting the actual model reduction
  while (min(ar.min, ma.min) < penalty){
    if (ar.min < ma.min){
      if (verbose) print(sprintf("AR out is: %i",which(ar.f ==ar.min)))
      ar.p[ar.f ==ar.min] <- FALSE
      if (verbose) print(ar.p)
    } else{
      if (verbose) print(sprintf("MA out is: %i",which(ma.f ==ma.min)))
      ma.p[ma.f ==ma.min] <- FALSE
      if (verbose)  print(ma.p)
    } # else
    if (ar.min != Inf){
      ar.p <- lead.one(ar.p, add = 1)
    }
    if (ma.min != Inf){
      ma.p <- lead.one(ma.p, add = 1)
    }
    ## Now restimate
    obj <- marima(data, ar.pattern =  ar.p,
                  ma.pattern =  ma.p, max.iter = max.iter)
    
    if (any(obj$ar.fvalues[,,-1] >0)){
      ar.f <- obj$ar.fvalues[,,-1]
      ar.p <- obj$out.ar.pattern[,,-1]
      ar.min <- min(ar.f[ar.f>0])
    } else{
      ar.p <- obj$out.ar.pattern
      ar.min <- Inf
    }
    if (any(obj$ma.fvalues[,,-1] >0)){
      ma.f <- obj$ma.fvalues[,,-1]
      ma.p <- obj$out.ma.pattern[,,-1]
      ma.min <- min(ma.f[ma.f>0])
    } else {
      ma.p <- obj$out.ma.pattern
      ma.min <- Inf
    }
    
  } # while
  return(obj)
}
step.slow.p <- function(object, data, p.value=0.05, max.iter=50, verbose=FALSE){
  ## object: A marima object
  ## data:   The same data as given as argument when fitting 'object'
  ## p.value: The significance level
  ## max.iter: Number of iterations before evaluating the penalty
  ## verbose: Extra printing so that one can see which terms ar left out in each iteration
  
  # Init
  
  obj <- object # So that the original object is returned if no reduction is needed.
  "[" <- function(x, ...) .Primitive("[")(x, ..., drop = FALSE)
  
  if (any(obj$ar.fvalues[,,-1] >0)){
    ar.pv <- obj$ar.pvalues[,,-1]
    ar.p <- obj$out.ar.pattern[,,-1]
    ar.pv.max <- max(ar.pv[ar.p==1])
  } else{
    ar.pv.max <- -1
    ar.p <- obj$out.ar.pattern
  }
  if (any(obj$ma.fvalues[,,-1] >0)){
    ma.pv <- obj$ma.pvalues[,,-1]
    ma.p <- obj$out.ma.pattern[,,-1]
    ma.pv.max <- max(ma.pv[ma.p==1])
  } else {
    ma.pv.max <- -1
    ma.p <- obj$out.ma.pattern
  }
  print(c(ar.pv.max, ma.pv.max))
  # Now starting the actual model reduction
  while (max(ar.pv.max, ma.pv.max) > p.value){
    if (ar.pv.max > ma.pv.max){
      if (verbose) print(sprintf("AR out is: %i",which(ar.f ==ar.min)))
      ar.p[ar.pv ==ar.pv.max] <- FALSE
      if (verbose) print(ar.p)
    } else{
      if (verbose) print(sprintf("MA out is: %i",which(ma.f ==ma.min)))
      ma.p[ma.pv ==ma.pv.max] <- FALSE
      if (verbose)  print(ma.p)
    } # else
    if (ar.pv.max != -1){
      ar.p <- lead.one(ar.p, add = 1)
    }
    if (ma.pv.max != -1){
      ma.p <- lead.one(ma.p, add = 1)
    }
    ## Now restimate
    obj <- marima(data, ar.pattern =  ar.p,
                  ma.pattern =  ma.p, max.iter = max.iter)
    if (any(obj$ar.fvalues[,,-1] >0)){
      ar.pv <- obj$ar.pvalues[,,-1]
      ar.p <- obj$out.ar.pattern[,,-1]
      ar.pv.max <- max(ar.pv[ar.p==1])
    } else{
      ar.pv.max <- -1
      ar.p <- obj$out.ar.pattern
    }
    if (any(obj$ma.fvalues[,,-1] >0)){
      ma.pv <- obj$ma.pvalues[,,-1]
      ma.p <- obj$out.ma.pattern[,,-1]
      ma.pv.max <- max(ma.pv[ma.p==1])
    } else {
      ma.pv.max <- -1
      ma.p <- obj$out.ma.pattern
    }
    print(c(ar.pv.max, ma.pv.max))
    
    
  } # while
  obj$call.ar.pattern <- object$call.ar.pattern
  obj$call.ma.pattern <- object$call.ma.pattern
  return(obj)
}
