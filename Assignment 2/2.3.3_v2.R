##################
###### 2.3 #######
##################

library("forecast")
library("comprehenr")
library("stringr")
library("ggplot2")
library("dplyr")
library("itsmr")
library("tidyverse")

# 2.2
histogram_plot <- function(eps_sd, phi_2, h_title) {
  p <- 2 # AR order
  d <- 0 # Integration order
  q <- 0 # MA order
  
  n = 300
  sims <- 100
  
  # first run of the model
  # the time series is given only by the errors
  model_1 <- Arima(ts(rnorm(n, mean=0, sd=eps_sd)), order=c(p,d,q), seasonal=c(0,0,0),
                   fixed=c(phi=c(1.5, -phi_2), include.mean=FALSE))
  {
    est_parameters <- c()
    for (i in 1:sims) {
        #for the 100 simulations
      real <- simulate(model_1, nsim=n)
      param <- arima(real, order = c(2,0,0), include.mean = FALSE, method = "ML")
      #param <- yw(real, 2) 
      
      est_parameters <- append(est_parameters, param$coef[2])
      #est_parameters <- append(est_parameters, param$se.phi[2])
      
    }
    est_parameters <- -1*est_parameters
  }
  ep_mean <- mean(est_parameters)
  ep_sd <- sd(est_parameters)
  
  margin <- qt(0.975, df=n-1)*ep_sd/sqrt(n)
  CI <- c(ep_mean-margin,ep_mean+margin )
  #CI <- c(quantile(est_parameters, 0.025),quantile(est_parameters, 0.975))
  
  # histogram
  df = data.frame(grp='phi_2', x=est_parameters)
  sp = ggplot(df, aes(x, fill=grp)) + geom_histogram(alpha=0.5, bins = 30, fill="darkblue")
  plots = sp + geom_vline(xintercept = CI[1], linetype="dotted",
                  color = "blue", size = 1.5) +
    geom_vline(xintercept = CI[2], linetype="dotted",
               color = "blue", size = 1.5) +
    geom_vline(xintercept = phi_2,
               color = "red", size = 1) +
    labs(y = "Frequency", x = "φ_2", title = h_title)
    
  
  return(plots)
}


#sd = 0.1, phi_2 = 0.52
histogram_plot(0.1, 0.52, "Histogram: φ_2=0.52, σ=0.1")

#sd = 0.1, phi_2 = 0.98
histogram_plot(0.1, 0.98, "Histogram: φ_2=0.98, σ=0.1" )

#sd = 5, phi_2 = 0.52
histogram_plot(100, 0.52, "Histogram: φ_2=0.52, σ=100")

#sd = 5, phi_2 = 0.98
histogram_plot(5, 0.98, "Histogram: φ_2=0.98, σ=5")

## 2.3
est_parameters <- function(eps_sd, phi_2) {
  p <- 2 # AR order
  d <- 0 # Integration order
  q <- 0 # MA order
  
  n = 300
  sims <- 100
  
  model_1 <- Arima(ts(rnorm(n, mean=0, sd = eps_sd)), order=c(p,d,q), seasonal=c(0,0,0),
                   fixed=c(phi=c(1.5, -phi_2), include.mean=FALSE))
  {
    est_parameters <- c()
    for (i in 1:sims) {
      real <- arima.sim(list(ar=c(1.5, -phi2),  order=c(2,0,0)), n=300, sd=eps_sd)
      param <- arima(real, order = c(2,0,0), include.mean = FALSE) #method = "ML")
      #param <- yw(real, 2) 
      
      est_parameters <- append(est_parameters, param$coef[2])
      #est_parameters <- append(est_parameters, param$se.phi[2])
      
    }
    est_parameters <- -1*est_parameters
    est_parameters <- sd(est_parameters)
  }
  return(est_parameters)
}

cbind(est_parameters(0.1,0.52),est_parameters(5,0.52))

## 2.3

# phi_2 = 0.52
n= 300
model_1 <- Arima(ts(rnorm(n, mean=0, sd = 0.1)), order=c(2,0,0), seasonal=c(0,0,0),
                 fixed=c(phi=c(1.5, -0.52), include.mean=FALSE))

sims <- 100
{
  Y_1 <- c()
  est_parameters <- c()
  for (i in 1:sims) {
    real <- simulate(model_1, nsim=n)
    param <- arima(real, order = c(2,0,0), include.mean = FALSE)#, method = "ML")
    
    
    Y_1 <- append(Y_1, real)  
    est_parameters <- append(est_parameters, param$coef[2])
    
  }
  Y_1 <- matrix(Y_1, ncol=sims)
  matplot(Y_1, type="l", main=str_glue("{sims} realization of the AR(2) model"))
}



var_Y1 <- apply(Y_1, 2, var)

# phi_2 = 0.98
model_2 <- Arima(ts(rnorm(n, mean=0, sd = 0.1)), order=c(2,0,0), seasonal=c(0,0,0),
                 fixed=c(phi=c(1.5, -0.98), include.mean=FALSE))
{
  Y_2 <- c()
  est_parameters <- c()
  for (i in 1:sims) {
    real <- simulate(model_2, nsim=n)
    param <- arima(real, order = c(2,0,0), include.mean = FALSE)#, method = "ML")
    
    Y_2 <- append(Y_2, real)  
    est_parameters <- append(est_parameters, param$coef[2])
    
  }
  Y_2 <- matrix(Y_2, ncol=sims)
  #matplot(Y_2, type="l", main=str_glue("{sims} realization of the AR(2) model"))
}

var_Y2 <- apply(Y_2, 2, var)

par(mfrow = c(2,1))
plot(var_Y1)
plot(var_Y2)

par(mfrow = c(2,1))
plot(acf(simulate(model_1, n)))

plot(acf(simulate(model_2, n)))


###### 2.5
phi_plot <- function(eps_sd, phi_2) {
  p <- 2 # AR order
  d <- 0 # Integration order
  q <- 0 # MA order
  
  n = 300
  sims <- 100
  
  model_1 <- Arima(ts(rnorm(n, mean=0, sd = eps_sd)), order=c(p,d,q), seasonal=c(0,0,0),
                   fixed=c(phi=c(1.5, -phi_2), include.mean=FALSE))
  {
    est_phi1 <- c()
    est_phi2 <- c()
    
    for (i in 1:sims) {
      real <- simulate(model_1, nsim=n)
      param <- arima(real, order = c(2,0,0), include.mean = FALSE)#, method = "ML")
      
      est_phi1 <- append(est_phi1, param$coef[1])
      est_phi2 <- append(est_phi2, param$coef[2])
      
    }
  }
  phi_pair <- cbind(-1*est_phi1, -1*est_phi2)
  
  phi_plots = plot(phi_pair, main=str_glue("Estimated phi-pairs with sigma^2 = {eps_sd}^2 and phi_2 = {phi_2}"), 
                   xlab = 'phi_1', ylab = 'phi_2')
  return(phi_plots)
  
}

#sd = 0.1, phi_2 = 0.52
phi_plot(0.1, 0.52)

#sd = 0.1, phi_2 = 0.98
phi_plot(0.1, 0.98)

#sd = 5, phi_2 = 0.52
phi_plot(5, 0.52)

#sd = 5, phi_2 = 0.98
phi_plot(5, 0.98)
  




