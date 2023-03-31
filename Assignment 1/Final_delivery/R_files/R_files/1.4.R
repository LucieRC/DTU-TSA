#### 1.4 #### 
# Optimal Lambda 
# In this question you should find the optimal λ 
# (minimizing squared one step prediction errors) with a burning
# period of 100 months.

# 1.4.1 # plot with optimal lambda showing the data and predictions from 2010
# and onwards

## loading data and defining variables 
data <- read.table("A1_co2.txt", header=TRUE) 
data_matrix <- as.matrix(data)

x <- data_matrix[1:738,3] 
Y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x))

matrix_train <- data_matrix[1:718,1:4]
x_train <- data_matrix[1:718,3] # burnin point of 100 months
y_train <- data_matrix[1:718,4] # burnin point of 100 months

## constants
l_step <- 1
n <- length(y_train) # number of observations
init <- 100 #  transient / burnin-point
p = 4 # number of parameters
lambda_list <- seq(from = 0.01, to = 1, by = 0.001) # list of different lambda

# define L matrix and f vector
p_harm <- 12
f <- function(j) rbind(1,j,sin(2*pi/p_harm*j),cos(2*pi/p_harm*j))
L <- t(matrix(c(1,0,0,0, 1,1,0,0, 0,0,cos(2*pi/p_harm),sin(2*pi/p_harm), 0,0,-sin(2*pi/p_harm),cos(2*pi/p_harm)),ncol=4))
LInv <- solve(L)

epsilon_list <- c()

# loop over all lambda 

for (g in 1:length(lambda_list)){
  print(g) # print iteration
  lambda <- lambda_list[g] # calculate with lambda[g]
  
  # FNinit & hNinit (burning point)
  F <- matrix(0, nrow=p, ncol=p)
  h <- matrix(0,nrow=p,ncol=1)
  
  for (j in 0:(init-1)){
    F <- F + lambda^j*f(-j)%*%t(f(-j))
    h <- h + lambda^j*f(-j)*y_train[init-j]
  }
  
  # allocating space
  theta.all <- matrix(NA,ncol=p, nrow=n)
  epsilon.all <- matrix(NA,nrow=n, ncol=1)
  sum_sqr_epsilon <- c()
  y_est <- matrix(NA,nrow=n,ncol=1)
  
  
  # solving at time init (100)
  theta.hat <- solve(F)%*%h
  theta.all[init,] <- theta.hat

  
  for (i in seq(from = init+1, to = n, by = l_step)){
    y_est[i] <- t(f(1)) %*% theta.hat
    F <- F + lambda^(i-1) * f(-i+1) %*% t(f(-i+1))
    h <- lambda* LInv %*% h + f(0)%*%y_train[i]
    
    theta.hat <- solve(F)%*%h  
    theta.all[i,] <- theta.hat
  
    residual <- y_train[i] - y_est[i]
    epsilon.all[i] <- residual

  }
  skip <- init+1
  sum_sqr_epsilon <- sum(epsilon.all[skip:n]^2) # sum of squarred error 
  epsilon_list <- cbind(epsilon_list, sum_sqr_epsilon)
  
}
# comment on the results 
plot(lambda_list, epsilon_list, type = 'l', xlim = c(0.01, 1), ylab = 'SSE', xlab = 'lambda', main='SSE as a function of lambda')

min(epsilon_list)
idx <- which.min(epsilon_list)
lambda_list[idx]

### it says optimal lamda is 1, let's try....

lambda <- 0.93

F <- matrix(0, nrow=p, ncol=p)
h <- matrix(0,nrow=p,ncol=1)

for (j in 0:(init-1)){
  F <- F + lambda^j * f(-j) %*% t(f(-j))
  h <- h + f(-j)*y_train[init-j]
}

# allocating space
theta.all <- matrix(NA,ncol=p, nrow=n)
sigma.all <- rep(NA, n)
sd.theta.all <- matrix(NA,ncol=p, nrow=n)
epsilon.all <- matrix(NA,nrow=n, ncol=1)


# solving at time init (0-100)
theta.hat <- solve(F)%*%h
theta.all[init,] <- theta.hat
epsilon.all[1:init] <- y_train[1:init] - t(f((init-1):0)) %*% theta.hat
sigma.all[init] <- sqrt(sum(epsilon.all[init]^2)/(init - p))
sd.theta.all[init,] <- sigma.all[init] * sqrt(diag(solve(F)))
y_est <- matrix(NA,nrow=n,ncol=1)


for (i in seq(from = init+1, to = n, by = l_step)){
  y_est[i] <- t(f(1)) %*% theta.hat
  F <- F + lambda^(i-1) * f(-i+1) %*% t(f(-i+1))
  h <- lambda* LInv %*% h + f(0)%*%y_train[i]
  
  theta.hat <- solve(F)%*%h  
  theta.all[i,] <- theta.hat
  
  epsilon.all[i] <- y_train[i] - y_est[i]
  sigma.all[i] <- sqrt(sum(epsilon.all[i]^2)/(i - p))
  sd.theta.all[i,] <- sigma.all[i] * sqrt(diag(solve(F)))
}

par(mfrow=c(1,1), mar=c(3,3,1,0.2),mgp=c(2,0.7,0))

plot(y_train, type="l", col = "red", xlim = c(0,750), ylim = c(200,420))
lines(y_est,col="green")

y_test <- matrix(NA,nrow=n,ncol=1)
c <- 0

for (i in seq(from = -n+1, to = 0, by = l_step)){
  c <- c+1
  y_test[c] <- t(f(i))%*%theta.hat[]

}
