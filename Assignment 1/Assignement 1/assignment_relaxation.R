install.packages("comprehenr")
library("comprehenr")

N <- 5 # Run a single simulation. Increase N to run multiple simulations
data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")
# data <- readLines("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv")

data_matrix <- as.matrix(data)
x <- data_matrix[1:738,3]
y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
n <- length(x)

Theta_hat <- matrix(, nrow=6, ncol=4)
# Theta_hat <- list()
# Theta_hat <- append(Theta_hat, list(-2.73243079e+03,  1.55187986e+00, -1.04225895e+00,  2.63447327e+00))#, 9.99682314e-01)
theta_hat1 <- c(-2.73243079e+03,  1.55187986e+00, -1.04225895e+00,  2.63447327e+00)#, 9.99682314e-01)
# Theta_hat <- append(Theta_hat, list(-2.73243079e+03,  1.55187986e+00, -1.04225895e+00,  2.63447327e+00))#, 9.99682314e-01)
# my_matrix = do.call(rbind, Theta_hat)
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
  return(as.numeric(Sigmay))
}

# estimated_y <- function(x, theta_hat){
#   return(theta_hat[1]+theta_hat[2]*x+theta_hat[3]*sin(2*pi/theta_hat[5]*x)+theta_hat[6]*cos(2*pi/theta_hat[5]*x))
# }

# est_y <- apply(x, 1, estimated_y, theta_hat)
# est_y <- X%*%theta_hat

compute_corr <- function(eps){
  #of eps and eps coupé décalé
  cor(eps[-1], eps[-length(eps)])
}

eps <- y - X%*%theta_hat1
rho <- compute_corr(eps)
# Sigmay <- build_cor_matrix(n, rho)
Sigmay <- build_cor_matrix(n, rho)#, ncol = 738)
# Sigmay <- matrix(as.numeric(build_cor_matrix(n, rho)), n, n)

OLSErrors <- WLSErrors <- c() 
# Feature matrix
# Sigmay <- diag(n) # initial guess of correlation structure
for(i in 1:5){ 
  print("Iteration",i)
  theta_hat <- solve(t(X)%*%solve(Sigmay)%*%X)%*%(t(X)%*%solve(Sigmay)%*%y) # Estimate parameters using currently assumed correlation structure
  eps <- y - X%*%theta_hat
  rho <- compute_corr(eps)
  Sigmay <- build_cor_matrix(n, rho)
  Theta_hat[i+1,] <- theta_hat
}

#Ouput:
# theta_hat
