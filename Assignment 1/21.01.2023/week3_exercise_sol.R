## Sample solution to addon exercise in week 3
## Lasse Engbo Christiansen

## A) Simulating data
time <- 1:200
Y <- 2e-3*(time)^2 - .35*time+ rnorm(200,mean = 0, sd= 3)
plot(time, Y)

## B) Specify a suitable global trend model for the data.

# Quadratic trend model ...Eq. 3.84 ... f(j) = c(1, j, j^2/2)
f <- function(j) rbind(1, j, j^2/2)
L <- matrix(c(1,1,0.5, 0,1,1, 0,0,1),ncol=3)
LInv <- solve(L)

## C) How many observations are needed before an estimate of theta 
##    can be obtained.

# 3 parameters so at 3 observations are needed to estimate theta ... 
# however one more is needed to get an estimate of the uncertainty.

## D) Make recursive estimation of the parameters. 
##    Plot the estimated parameters over time (Include a measure of uncertainty). 
##    Hint: Skip estimating for the first 5 observations.

n <- length(Y)
init <- 5
## FNinit & hNinit (First observations)
F <- matrix(0, nrow=3, ncol=3)
h <- matrix(0,nrow=3,ncol=1)
for (j in 0:(init-1)){
  F <- F + f(-j)%*%t(f(-j))
  h <- h + f(-j)*Y[init-j]
}

## Allocating space
p <- length(h)
theta.all <- matrix(NA,ncol=p, nrow=n)
sigma.all <- rep(NA, n)
sd.theta.all <- matrix(NA,ncol=p, nrow=n)
## Solving at time init
theta.hat <- solve(F, h)
theta.all[init,] <- solve(F, h)
epsilon <- Y[1:init] - cbind(1, -(init-1):0, (-(init-1):0)^2/2) %*% theta.hat
sigma.all[init] <- sqrt(sum(epsilon^2)/(init - p))
sd.theta.all[init,] <- sigma.all[init] * sqrt(diag(solve(F)))

## Looping over the remaining observations
for (i in (init+1):n){
  F <- F + f(- (i-1)) %*% t(f(- (i-1)))
  h <- LInv %*% h + f(0)*Y[i]
  theta.hat <-  solve(F, h)  
  theta.all[i,] <- theta.hat

  ## Adding uncertainty information
  ## Two options for estimating sigma: 
  ## A) using latest theta (Best) or B) updating previous estimate (Least calculations)
  ## A)
  epsilon <- Y[1:i] - cbind(1, -(i-1):0, (-(i-1):0)^2/2) %*% theta.hat
  sigma.all[i] <- sqrt(sum(epsilon^2)/(i - p))
  ## B)
  # sigma.all[i] <- sqrt( (sigma.all[i-1]^2*(i-1-p) + (Y[i]-theta.hat[1])^2)/(i - p) )
  ## Estimating s.d. of estimated parameters
  sd.theta.all[i,] <- sigma.all[i] * sqrt(diag(solve(F)))
}
## From looking at the following:
par(mfrow=c(3,1), mar=c(3,3,1,0.2),mgp=c(2,0.7,0))
matplot(theta.all)
matplot(sd.theta.all)
plot(sigma.all,col=4,lwd=3)
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
