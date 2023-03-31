data <- read.csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")
data_matrix <- as.matrix(data)
x_train <- data_matrix[1:718,3]
y_train <- data_matrix[1:718,4]


#Build x_N
p_harm = 1.1
n = 718
# f <- function(j) t(cbind(1,j,sin(2*pi/p_harm*j),cos(2*pi/p_harm*j)))

x_N <- cbind(1,x_train,sin(2*pi*x_train),cos(2*pi*x_train))

theta_N <- solve(t(x_N)%*%x_N)%*%t(x_N)%*%y_train

y_est <- x_N%*%theta_N
write.csv(y_est,"C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_from_book.csv")
