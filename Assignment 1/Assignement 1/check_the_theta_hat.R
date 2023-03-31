data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")

data_matrix <- as.matrix(data)
x <- data_matrix[1:738,3]
y <- data_matrix[1:738,4]
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x)) # ça marche
n <- length(x)
matrix_train <- data_matrix[1:718,1:4]

theta_hat <- t(t(c(-4991.4716733,2.6756658,3.2177609,-0.7783417)))
theta_hat

return_f <- function(N,j){
    # print(N)
    # print(j)
    # if(N==0) j_time_value <- matrix_train[1,3]
    # else{
    j_time_value <- matrix_train[N+j,3] #}
    return(t(cbind(1, j_time_value, sin(2*pi*j_time_value), cos(2*pi*j_time_value))))
}

Y <- matrix(0,718,1)
for (i in 1:718){
    Y[i] <- t(return_f(718,-718+i))%*%theta_hat
}

write.csv(Y,file="Y_estimated.csv")
