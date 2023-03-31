data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")

data_matrix <- as.matrix(data)
x <- data_matrix[1:738,3]
y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
n <- length(x)
matrix_train <- data_matrix[1:718,1:4]


####
lambda <- 0.9

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

return_f <- function(N,j){
    # print(N)
    # print(j)
    # if(N==0) j_time_value <- matrix_train[1,3]
    # else{
    j_time_value <- matrix_train[N+j,3] #}
    return(t(cbind(1, j_time_value, sin(2*pi*j_time_value), cos(2*pi*j_time_value))))
}

return_L <- function(){
  L = matrix(0,4,4)
  diag(L) = 1
  L[2,1] = 1
}

# return_F_N <- function(lambda, N){
#     return(sum(lambda**j*return_f(-j)*t(return_f(-j)) for(j in {1:(N-1)}))) #for(i in {x <- 1:100;x[x%%2 == 0]})
# }


compute_F_for_theta <- function(N, lambda){
    print("Execution F")
    F = matrix(0,4,4) 
    if(N==0) return(F)
    else{
        for(j in 0:(N-1)){
            F = F + lambda**j*return_f(N,-j)%*%t(return_f(N,-j))}}
    return(F)
}

compute_h_for_theta <- function(N, lambda){
    print("Execution h")
    h = matrix(0,4,1)
    if(N==0) return(h)
    else{
        for(j in 0:(N-1)) h = h + lambda**j*return_f(N,-j)%*%matrix_train[N-j,4]}
    # what about j in 1:N ?
    return(h)
}

compute_theta <- function(N, lambda){
  # N = 0:718 -> the rows of the matrix
#   print("Execution theta")
  theta = solve(compute_F_for_theta(N,lambda)) %*% compute_h_for_theta(N,lambda)
  return(theta)
}

theta_hat <- t(t(c(-4991.4716733,2.6756658,3.2177609,-0.7783417)))

compute_list_Y <- function(N,lambda){
    Y <- matrix(0,N,1)
    for(j in 0:(N-1)){
        Y[j+1] <- t(return_f(N,-j))%*%theta_hat
    }

}

#Je pense qu'il faut le compute au fur et à mesure
compute_theta(1,lambda)
compute_theta(0,lambda)


compute_theta <- function(N, lambda){
  # N = 0:718 -> the rows of the matrix
#   print("Execution theta")
  theta = solve(compute_F_for_theta(N,lambda)) %*% compute_h_for_theta(N,lambda)
  return(theta)
}

Y_hat_moins_717 = t(return_f(718,-717))%*%theta_hat ; Y_hat_moins_717
Y_hat_moins_1 = t(return_f(718,-1))%*%theta_hat ; Y_hat_moins_1
Y_hat_plus_1 = t(return_f(718,1))%*%theta_hat ; Y_hat_moins_1
# ça va pas marcher par rapport au dataset

compute_Sigma <- function(N,lambda){
  Sigma <- matrix(0,N,N)
  for (i in 1:N){
    Sigma[i,i] <- 1/(lambda**(N-i))
  }
  return(Sigma)
}

compute_Sigma_inv <- function(N,lambda){
  Sigma <- matrix(0,N,N)
  for (i in 1:N){
    Sigma[i,i] <- lambda**(N-i)
  }
  return(Sigma)
}

compute_x_N <- function(N){
  x_N <- matrix(0,1,4)
  for (i in 1:718){
    x_N <- rbind(x_N, t(return_f(N,-N+i)))
  }
  return(x_N[-1,]) #to delete first row
}

compute_sigma <- function(N,lambda){
  p <- 4
  T <- 0
  for(j in 0:(N-1)){
    T = T + lambda**j}
  theta_hat <- t(t(c(-4991.4716733,2.6756658,3.2177609,-0.7783417)))
  Y <- matrix_train[1:N,4]
  x_N <- compute_x_N(N)
  mat <- Y - x_N%*%theta_hat
  Sigma_inv <- compute_Sigma_inv(N,lambda)

  sigma <- t(mat)%*%Sigma_inv%*%mat
  sigma <- 1/(T-p)*sigma
  return(sigma)
}
compute_sigma(718,0.9)
