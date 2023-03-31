##################
###### 2.3 #######
##################

library("forecast")
#install.packages("comprehenr")
library("comprehenr")
#install.packages("stringr")
library("stringr")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("dplyr")
library("dplyr")

# Simulating an AR(2) with phi_2 = 0.52 and sigma^2 = 0.1^2
p <- 2 # AR order
d <- 0 # Integration order
q <- 0 # MA order

n = 300

set_sd = 5
set_phi2 = 0.98

phi2_list <- seq(0,10,0.1)
var_phi2_distrib_list <- c()

for (phi2 in phi2_list) {
    model_1 <- Arima(ts(rnorm(n, mean=0, sd=set_sd)), order=c(p,d,q), seasonal=c(0,0,0),
                fixed=c(phi=c(1.5, -phi2), include.mean = F))

    sims <- 100
    {
    Y_1 <- c()
    est_parameters <- c()
    for (i in 1:sims) {
        real <- simulate(model_1, nsim=n)
        param <- arima(real, order=c(2,0,0), include.mean=FALSE)
        
        Y_1 <- append(Y_1, real)  
        est_parameters <- append(est_parameters, param$coef[2])
        
    }
    Y_1 <- matrix(Y_1, ncol=sims)
    
    # matplot(Y_1, type="l", main=str_glue("{sims} realization of the AR(2) model"))
    }

    # hist(est_parameters)

    # quantile(est_parameters, 0.95)
    # geom_histogram(est_parameters, binwidth=1)

    # df = data.frame(grp='phi2_1', x=est_parameters)
    # sp = ggplot(df, aes(x, fill="phi_2")) + geom_histogram(alpha=0.5)
    # sp + geom_vline(xintercept=quantile(est_parameters, 0.95), linetype="dotted",
    #                 color="blue", size=1.5) + geom_vline(xintercept=quantile(est_parameters, 0.05), linetype="dotted",
    #                 color="blue", size=1.5) + ggtitle("(phi_2=0.98, sigma=5) ; var=0.00013") + theme_bw()
    var_phi2_distrib_list <- append(var_phi2_distrib_list, var(est_parameters))
}


for (phi2 in phi2_list) {
    model_1 <- Arima(ts(rnorm(n, mean=0, sd=set_sd)), order=c(p,d,q), seasonal=c(0,0,0),
                fixed=c(phi=c(1.5, -0.52), include.mean = F))
    sims <- 100
    est_parameters
    {
    Y_1 <- c()
    est_parameters <- c()
    for (i in 1:sims) {
        real <- simulate(model_1, nsim=n)
        param <- arima(real, order=c(2,0,0), include.mean=FALSE)
        
        Y_1 <- append(Y_1, real)  
        est_parameters <- append(est_parameters, param$coef[2])
    }
    Y_1 <- matrix(Y_1, ncol=sims)
    }
    var_phi2_distrib_list <- append(var_phi2_distrib_list, var(est_parameters))
}







 
par(mfrow=c(1,2))

acf(Y_1,main="ACF")
pacf(Y_1,main="PACF")

# with sigma^2 = 5^2

model_2 <- Arima(ts(rnorm(n, mean=0, sd = 5)), order=c(p,d,q), seasonal=c(0,0,0),
               fixed=c(phi=c(1.5, -0.52), include.mean = F))

Y_2 <- simulate(model_2, nsim=n)

acf(Y_2,main="ACF")
pacf(Y_2,main="PACF")

par(mfrow=c(2,1))

plot(Y_1)
plot(Y_2)


# Simulating an AR(2) with phi_2 = 0.52 and sigma^2 = 0.1^2
p <- 2 # AR order
d <- 0 # Integration order
q <- 0 # MA order

n =3

model_3 <- Arima(ts(rnorm(n, mean=0, sd = 0.1)), order=c(p,d,q), seasonal=c(0,0,0),
                 fixed=c(phi=c(1.5, -0.52), include.mean = F))

Y_3 <- simulate(model_1, nsim=n)

par(mfrow=c(1,2))

acf(Y_1,main="ACF")
pacf(Y_1,main="PACF")

# with sigma^2 = 5^2

model_4 <- Arima(ts(rnorm(n, mean=0, sd = 5)), order=c(p,d,q), seasonal=c(0,0,0),
                 fixed=c(phi=c(1.5, -0.52), include.mean = F))

Y_4 <- simulate(model_2, nsim=n)

acf(Y_2,main="ACF")
pacf(Y_2,main="PACF")

par(mfrow=c(2,1))

plot(Y_1)
plot(Y_2)