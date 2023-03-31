## Sample solution to addon exercise in week 3
## Lasse Engbo Christiansen

## A) Simulating data
data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")

data_matrix <- as.matrix(data)
# x <- data_matrix[1:738,3]
# y <- data_matrix[1:738,4]
# X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
# n <- length(x)
matrix_train <- data_matrix[1:718,1:4]
x_train <- data_matrix[1:718,3]
y_train <- data_matrix[1:718,4]

## B) Specify a suitable global trend model for the data.

# Quadratic trend model ...Eq. 3.84 ... f(j) = c(1, j, j^2/2)
p_harm <- 12
f <- function(j) t(cbind(1,j,sin(2*pi/p_harm*j),cos(2*pi/p_harm*j)))
L <- t(matrix(c(1,0,0,0, 1,1,0,0, 0,0,1,0, 0,0,0,1),ncol=4))
LInv <- solve(L)

## C) How many observations are needed before an estimate of theta 
##    can be obtained.

# 3 parameters so at 3 observations are needed to estimate theta ... 
# however one more is needed to get an estimate of the uncertainty.

## D) Make recursive estimation of the parameters. 
##    Plot the estimated parameters over time (Include a measure of uncertainty). 
##    Hint: Skip estimating for the first 5 observations.

n <- length(y_train)
init <- 10
p = 4
lambda <- 0.9
## FNinit & hNinit (First observations)
F <- matrix(0, nrow=4, ncol=4)
h <- matrix(0,nrow=4,ncol=1)
for (j in 0:(init-1)){
  F <- F + lambda**j * f(-j) %*% t(f(-j))
  h <- h + f(-j)*y_train[init-j]
}

## Allocating space
theta.all <- matrix(NA,ncol=p, nrow=n)
sigma.all <- rep(NA, n)
sd.theta.all <- matrix(NA,ncol=p, nrow=n)
## Solving at time init
theta.hat <- solve(F)%*%h
theta.all[init,] <- solve(F)%*%h
epsilon <- y_train[1:init] - cbind(1, -(init-1):0, (-sin(2*pi*(init-1):0)), -cos(2*pi*(init-1):0)) %*% theta.hat
sigma.all[init] <- sqrt(sum(epsilon^2)/(init - p))
sd.theta.all[init,] <- sigma.all[init] * sqrt(diag(solve(F)))

## Looping over the remaining observations
for (i in (init+1):n){ #normally 11:718
  F <- F + lambda**i * f(-(i-1)) %*% t(f(-(i-1)))
  h <- lambda * LInv %*% h + f(0)*y_train[i]
  theta.hat <-  solve(F) %*% h 
  theta.all[i,] <- theta.hat

  ## Adding uncertainty information
  ## Two options for estimating sigma: 
  ## A) using latest theta (Best) or B) updating previous estimate (Least calculations)
  ## A)
  epsilon <- y_train[1:i] - cbind(1, -(i-1):0, (-sin(2*pi*(i-1):0)), -cos(2*pi*(i-1):0)) %*% theta.hat
  sigma.all[i] <- sqrt(sum(epsilon^2)/(i - p))
  ## B)
  # sigma.all[i] <- sqrt( (sigma.all[i-1]^2*(i-1-p) + (Y[i]-theta.hat[1])^2)/(i - p) )
  ## Estimating s.d. of estimated parameters
  sd.theta.all[i,] <- sigma.all[i] * sqrt(diag(solve(F)))
}

estimated_y <- function(){
    N = 718
    y_est <- matrix(NA,N,3)
    for(i in init+1:(N-10)){
        # print(i)
        y_est[i,1] <- t(f(-N+i)) %*% theta.all[i,1:4]
    }
    return(y_est)
}
y_est <- estimated_y()
head(y_est)

## From looking at the following:
par(mfrow=c(4,1))#, mar=c(3,3,1,0.2),mgp=c(2,0.7,0))
matplot(theta.all)
matplot(sd.theta.all)
plot(sigma.all,col=4,lwd=3)
plot(y_train)

#My way
par(mfrow=c(2,1))#, mar=c(3,3,1,0.2),mgp=c(2,0.7,0))
# matplot(theta.all)
matplot(sd.theta.all)
plot(sigma.all,col=4,lwd=3)
write.csv(sd.theta.all,"sd.theta.all.csv")
write.csv(sigma.all,"sigma.all.csv")
write.csv(y_est,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_local_trend.csv")



# Predict n step ahead, i.e. for time 501:550
predict.int <- numeric(n) #n zeros
## (3.61)
for(i in 1:n){
  predict.int[i] <-
    qt(0.975, n-p) * sigma.all[i] * #inverse cumulative density function
    sqrt(1 + t(f(-i)) %*% solve(F) %*% f(-i))
}

# Prediction intervals 
y_est[,2] <- y_est[,1] - predict.int
y_est[,3] <- y_est[,1] + predict.int
tail(y_est)

write.csv(y_est,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_local_trend.csv")





















## Then it is suggested to make individual plots for each parameter
t.quan <- qt(p = 0.975, df=(1:n)-p ) # Does produce Not-a-Number
t.quan[1:init] <- NA

par(mfrow=c(3,1), mar=c(3,3,1,0.2),mgp=c(2,0.7,0))
for (th in 1:3)
  matplot(theta.all[,th] + t.quan * cbind(0,-sd.theta.all[,th],sd.theta.all[,th]), type="l",lty=c(1,2,2),lwd=2,col=th,ylab=paste("Theta",th))

## E) Compare the last estimate of theta with a general linear model estimation
lm1 <- lm(Y ~ time + I(time^2))
summary(lm1)
lm2 <- update(lm1, .~.-1)
summary(lm2)
theta.all[n,]
## Well not that easy to compare ... trying again with shifted time
lm1t <- lm(Y ~ I(time-n) + I((time-n)^2))
summary(lm1t)
## Same model but: Much easier ... note the (expected) factor 2 difference 
## for the third parameter.



##  
theta.hat
XN <- cbind(1,-199:0, (-199:0)^2/2)
yhat <- XN%*%theta.hat
par(mfrow=c(1,1))
plot(1:200,Y)
lines(1:200,yhat)
