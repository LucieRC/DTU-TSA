library("forecast")
source("step.slow.marima_2017.R")
source("step.slow.p.marima_2017.R")
library(marima)

A3Data_full <- read.csv("A3Data.csv", head=T)
A3Data <- A3Data_full[1:122,]

# chose columns to investigate
selects <- c(3,4,5,6)
HousePrices <- data.frame(A3Data[,selects])
HousePrices_full <- data.frame(A3Data_full[,selects])
colnames(HousePrices) <- colnames(A3Data)[selects]
kvar <- length(selects) + 2
k <- c(1:kvar)


# log transform
log.HousePrices <- log(HousePrices)
log.HousePrices$Interest <- A3Data$InterestRate[1:122]
log.HousePrices$Inflation <- A3Data$InflationRate[1:122]

# difference data

difs <- rep(1, kvar)
difference<-c(1,1, 2,1, 3,1, 4,1) 

dlog.HousePrices <- t(define.dif(log.HousePrices,difference)$y.dif)

# acf & pacf
acf(dlog.HousePrices[,1:4])
pacf(dlog.HousePrices[,1:4])

#
#ar(1,2), ma(2,4,8,12)

ar <-c(1)
ma <-c(2,4,8)

mod <- define.model(kvar = kvar, ar=ar, ma=ma, reg.var=c(6,5), indep=NULL)
Model <- marima(dlog.HousePrices, means = 1, ar.pattern=mod$ar.pattern, ma.pattern=mod$ma.pattern,
                Check=FALSE, Plot = "trace", penalty=2)

slp <- step.slow.p(Model, dlog.HousePrices, p.value = 0.05)

Forecasts <-  arma.forecast(t(rbind(ts(dlog.HousePrices), matrix(NA,6,kvar))), nstart=(dim(dlog.HousePrices)[1]), nstep=6, marima=slp)

forecast_original <- list()
for (i in seq(1,4,1)) {
  len <- length(forecast_original)
  forecast_original[[len+1]] <- exp(diffinv(Forecasts$forecasts[i,], xi=log(HousePrices[1,i])))
}
HousePrices_full$time <- A3Data_full$X
HousePrices_full$Capital_forecast <- unlist(forecast_original[1])
HousePrices_full$Sealand_forecast <- unlist(forecast_original[2])
HousePrices_full$MidJutland_forecast <- unlist(forecast_original[3])
HousePrices_full$Rural_forecast <- unlist(forecast_original[4])


# Cool plot but without a legend because I don't know how
ggplot(HousePrices_full, aes(time)) + 
  geom_point(aes(y=Capital),col="red") +
  geom_line(aes(y = Capital_forecast, group = 1),col = "red") +
  geom_point(aes(y=Sealand),col="green") +
  geom_line(aes(y = Sealand_forecast, group = 1),col = "green") +
  geom_point(aes(y=MidJutland),col="blue") +
  geom_line(aes(y = MidJutland_forecast, group = 1),col = "blue") +
  geom_point(aes(y=Rural),col="purple") +
  geom_line(aes(y = Rural_forecast, group = 1),col = "purple") +
  scale_x_discrete(labels = everysecond(Everything$time)) +
  ylim(300,5500) +
  ylab("Price") +
  xlab("Time") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#Cringe plot
{
  plot(exp(diffinv(dlog.HousePrices[,1],xi=c(log(HousePrices[1,1])))), ylim=c(0,5500))
  points(exp(diffinv(dlog.HousePrices[,2],xi=c(log(HousePrices[1,2])))), col="red")
  points(exp(diffinv(dlog.HousePrices[,3],xi=c(log(HousePrices[1,3])))), col="blue")
  points(exp(diffinv(dlog.HousePrices[,4],xi=c(log(HousePrices[1,4])))), col="green")
  
  lines(exp(diffinv(Forecasts$forecasts[1,],xi=c(log(HousePrices[1,1])))))
  lines(exp(diffinv(Forecasts$forecasts[2,],xi=c(log(HousePrices[1,2])))), col="red")
  lines(exp(diffinv(Forecasts$forecasts[3,],xi=c(log(HousePrices[1,3])))), col="blue")
  lines(exp(diffinv(Forecasts$forecasts[4,],xi=c(log(HousePrices[1,4])))), col="green")
  legend("topleft", title="Original data", legend=c("Capital", "Sealand", "MidJutland", "Rural"),
        col=c("black", "red", "blue", "green"), pch=1, cex=0.8)
  legend(-3.9,4500, title="Predicted data", legend=c("Capital", "Sealand", "MidJutland", "Rural"),
         col=c("black", "red", "blue", "green"), lty=1, cex=0.8)
}


# I don't think this is correct as it does not arrive at the original values
{
  plot(exp(diffinv(dlog.HousePrices[,1]))*1000)
  points(exp(diffinv(dlog.HousePrices[,2]))*1000, col="red")
  points(exp(diffinv(dlog.HousePrices[,3]))*1000, col="blue")
  points(exp(diffinv(dlog.HousePrices[,4]))*1000, col="green")
  
  lines(exp(diffinv(Forecasts$forecasts[1,]))*1000, col='black')
  lines(exp(diffinv(Forecasts$forecasts[2,]))*1000, col='red')
  lines(exp(diffinv(Forecasts$forecasts[3,]))*1000, col='blue')
  lines(exp(diffinv(Forecasts$forecasts[4,]))*1000, col='green')
}








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
