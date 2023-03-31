data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")
data_matrix <- as.matrix(data)
matrix_train <- data_matrix[1:718,1:4]
x_train <- data_matrix[1:718,3]
n <- length(x_train)
y_train <- matrix(data_matrix[1:718,4],nrow=n,ncol=1)
p <- 4
lambda <- 0.90

p_harm <- 12
f <- function(j) rbind(1,j,sin(2*pi/p_harm*j),cos(2*pi/p_harm*j))
L <- t(matrix(c(1,0,0,0, 1,1,0,0, 0,0,cos(2*pi/p_harm),sin(2*pi/p_harm), 0,0,-sin(2*pi/p_harm),cos(2*pi/p_harm)),ncol=4))
LInv <- solve(L)


init <- 10
## FNinit & hNinit (First observations)
F <- matrix(0, nrow=p, ncol=p)
h <- matrix(0, nrow=p, ncol=1)
for (j in 0:(init-1)){
  F <- F + lambda^j*f(-j)%*%t(f(-j))
  h <- h + lambda^j*f(-j)*y_train[init-j]
}

## Allocating space
theta.all <- matrix(NA,ncol=p, nrow=n)
sigma.all <- rep(NA, n)
sd.theta.all <- matrix(NA,ncol=p, nrow=n)
epsilon.all <- matrix(NA,nrow=n)
## Solving at time init
theta.hat <- solve(F)%*%h
theta.all[init,] <- solve(F)%*%h
epsilon <- y_train[1:init] - cbind(1, -(init-1):0, (-sin(2*pi/p_harm*(init-1):0)), -cos(2*pi/p_harm*(init-1):0)) %*% theta.hat
sigma.all[init] <- sqrt(sum(epsilon^2)/(init-p))
sd.theta.all[init,] <- sigma.all[init] * sqrt(diag(solve(F)))
y_est <- matrix(NA,nrow=n+20,ncol=1)
y_pred_up <- matrix(NA, nrow=n+20, ncol=1)
y_pred_down <- matrix(NA, nrow=n+20, ncol=1)

## Looping over the remaining observations
begin <- init+1
for (i in begin:n){
  y_est[i] <- t(f(1)) %*% theta.hat
  F <- F + lambda^(i-1) * f(-i+1) %*% t(f(-i+1))
  h <- lambda* LInv %*% h + f(0)%*%y_train[i]
  theta.hat <- solve(F)%*%h  
  theta.all[i,] <- theta.hat
  epsilon <- y_train[1:i] - cbind(1, -(i-1):0, (-sin(2*pi/p_harm*(i-1):0)), -cos(2*pi/p_harm*(i-1):0)) %*% theta.hat
  epsilon_for_sigma <- y_train[i] - y_est[i]
  espilon_continu <- y_train[begin:i] - y_est[begin:i]
  sigma <- sqrt((epsilon_for_sigma^2)/(i - p))
  sigma.all[i] <- sqrt(sum(epsilon^2)/(i - p))
  sd.theta.all[i,] <- sigma.all[i] * sqrt(diag(solve(F)))
  if(i>11){
    Sigma_for_pred <- t(espilon_continu)%*%diag(lambda^((i-1):init))%*%(espilon_continu)/(sum(lambda^((i-1):0))-p)
    y_pred_up[i] <- y_est[i] + qt(p=0.975, i-1-p)*sqrt(Sigma_for_pred*(1+t(f(1))%*%solve(F)%*%f(1)))
    y_pred_down[i] <- y_est[i] - qt(p=0.975, i-1-p)*sqrt(Sigma_for_pred*(1+t(f(1))%*%solve(F)%*%f(1)))
  }
}

for(i in 718:738){
  step = i-718
  y_est[i] <- t(f(step)) %*% theta.hat
  y_pred_up[i] <- y_est[i] + qt(p=0.975, i-1-p)*sqrt(Sigma_for_pred*(1+t(f(step))%*%solve(F)%*%f(step)))
  y_pred_down[i] <- y_est[i] - qt(p=0.975, i-1-p)*sqrt(Sigma_for_pred*(1+t(f(step))%*%solve(F)%*%f(step)))
}

write.csv(y_est,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_est_lambda.csv")
write.csv(y_pred_down,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_down.csv")
write.csv(y_pred_up,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_up.csv")
write.csv(epsilon.all,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/epsilon.all.csv")
write.csv(sd.theta.all,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/sd.theta.all.csv")
write.csv(sigma.all,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/sigma.all.csv")

y_pred1 <- t(f(1)) %*% theta.hat
y_pred2 <- t(f(2)) %*% theta.hat
y_pred6 <- t(f(6)) %*% theta.hat
y_pred12 <- t(f(12)) %*% theta.hat
y_pred20 <- t(f(20)) %*% theta.hat
