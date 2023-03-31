library("ggplot2")
library("comprehenr")

setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/R_files")


# co2_data <- read.table("A1_co2.txt", header=TRUE)
# split <- 2016
# datasplit <- co2_data[co2_data$time<=split,]

data <- read.csv("../data.csv", sep=" ")
# data_matrix <- as.dataframe(data)
data_train <- data[1:718,1:4]
# x_train <- data_matrix[1:718,3]

## 1.2.1
model <- lm(co2 ~ 1 + time + I(sin((2*pi/1)*time)) + I(cos((2*pi/1)*time)), data=data_train)
summary(model)

## 1.2.5
n <- length(data_train$time)
rho <- 0.5

build_cor <- function(n, rho){
  Sigmay <- vector("list", n)
  for (i in 1:n) {
    rows <- to_vec(for(k in 1:n) ifelse(k<=i, 0, rho^(k-i)))
    Sigmay <- rbind(Sigmay, rows)  
  }
  Sigmay <- Sigmay[-1,]
  
  diag <- rep(1,n)
  diag(Sigmay) <- diag
  Sigmay[lower.tri(Sigmay)] <- t(Sigmay)[lower.tri(Sigmay)]
  Sigmay <- as.numeric(Sigmay)
  Sigmay <- matrix(Sigmay, nrow=n, ncol=n)
  return(Sigmay)
  
}

Sigmay <- build_cor(n, rho)

# 1.2.5 - the algorithm
x <- data_train$time
X <- cbind(1,x,sin(2*pi*x),cos(2*pi*x))
# x_new <- news$time
# X_new <- cbind(1,x_new,sin(2*pi*x_new),cos(2*pi*x_new))
y <- data_train$co2
resids <- residuals(model)
rho <- cor(resids[-1], resids[-length(resids)])
thets <- c()
rho <- 1
for(i in 1:5){ 
  thetahat <- solve(t(X)%*%solve(Sigmay)%*%X)%*%(t(X)%*%solve(Sigmay)%*%y)
  eps <- y - X%*%thetahat
  rho <- cor(eps[-1], eps[-length(eps)])
  Sigmay <- build_cor(n, rho)
  print(i)
  thets <- cbind(thets, thetahat)
}
# WLS_preds <- append(X%*%thetahat, X_new%*%thetahat)
write.csv(thets,"../created_data/1.2.6_theta_from_WLS")

