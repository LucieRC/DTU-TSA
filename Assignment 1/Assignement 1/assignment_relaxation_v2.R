install.packages("comprehenr")
library("comprehenr")

N <- 5
data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")

data_matrix <- as.matrix(data)
x <- data_matrix[1:738,3]
y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
n <- length(x)

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
