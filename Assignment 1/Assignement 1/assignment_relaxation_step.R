install.packages("comprehenr")
library("comprehenr")

N <- 5
data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")

data_matrix <- as.matrix(data)
x <- data_matrix[1:738,3]
y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
n <- length(x)
matrix_train <- data_matrix[1:718,1:4]

Theta_hat <- matrix(, nrow=6, ncol=4)
theta_hat1 <- c(-2.73243079e+03,  1.55187986e+00, -1.04225895e+00,  2.63447327e+00)#, 9.99682314e-01)
Theta_hat[1,] <- theta_hat1


build_cor_matrix <- function(n, rho){
  Sigmay <- vector("list", n)
  for (i in 1:n) {
    rows <- to_vec(for(k in 1:n) ifelse(k<=i, 0, rho^(k-i)))
    Sigmay <- rbind(Sigmay, rows)
  }
  Sigmay <- Sigmay[-1,]
  diag <- rep(1,n)
  diag(Sigmay) <- diag
  Sigmay[lower.tri(Sigmay)] <- t(Sigmay)[lower.tri(Sigmay)]
  Sigmay <- matrix(as.numeric(Sigmay),738,738)
  return(Sigmay)
}

compute_corr <- function(eps){
  #of eps and eps coupé décalé
  cor(eps[-1], eps[-length(eps)])
}

eps <- y - X%*%theta_hat1
rho <- compute_corr(eps)
Sigmay <- build_cor_matrix(n, rho)

for(i in 1:5){ 
  sprintf("Iteration %.1f",i)
  theta_hat <- solve(t(X)%*%solve(Sigmay)%*%X)%*%(t(X)%*%solve(Sigmay)%*%y) # Estimate parameters using currently assumed correlation structure
  eps <- y - X%*%theta_hat
  rho <- compute_corr(eps)
  Sigmay <- build_cor_matrix(n, rho)
  Theta_hat[i+1,] <- theta_hat
}



nb_pred = 20 #entre 2018 et 2019
for(i in 1:nb_pred){
  predict.int[i] <-
    qt(0.95, 718-4) * sigma.hat * #inverse cumulative density function
    sqrt((1+t(c(1,500+i)) %*% solve(t(X)%*%X) %*% c(1,500+i)))
}



####
lambda = 0.9

return_theta_N <- function(
    #compiler les f

    return(t(return_f()))
)

return_f <- function(j){
  #
  return(cbind(1,j,sin(2*pi*j),cos(2*pi*j)))
}

return_X <- function(N){

}

return_L <- function(){
  L = matrix(0,4,4)
  diag(L) = 1
  L[2,1] = 1
  return(L)
}

return_F_N <- function(lambda, N){
    return(sum(lambda**j*return_f(-j)*t(return_f(-j)) for(j in {1:(N-1)}))) #for(i in {x <- 1:100;x[x%%2 == 0]})
}

est_Y_steps <- function(j){


}


compute_F_for_theta <- function(N, lambda){
  # seems good in terms of dimensions and code
  if N=0:
    F = matrix(0, 4, 4)
  else:
    F = compute_F_for_theta(N-1,lambda) + lambda**N*return_f(-N)%*%t(return_f(-N))
  return(F)
}

compute_h_for_theta <- function(N, lambda){
  # seems good in terms of dimensions
  if N=0:
    h = matrix(0, 4, 1)
  else:
    h = lambda*solve(return_L())*compute_h_for_theta(N-1,lambda) + return_f(0)%*%matrix_train[N,3]
  return(h)
}

compute_theta <- function(N, lambda){
  # N = 0:718 -> the rows of the matrix
  theta = solve(compute_F_for_theta(N,lambda))%*%compute_h_for_theta(N,lambda)
  return(theta)
}

