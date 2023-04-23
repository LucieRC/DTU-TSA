# Assignment 4
library(ggplot2)
setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 4")

data <- read.csv("A4_Kulhuse.csv") ; head(data)


plot(seq(1,10,1),data[1:10,1], type="l")
plot(seq(1,10,1),data[1:10,2])
for (i in 2:8){
  lines(na.omit(data[1:10,i]))
}

data[1:5,]
data$idx <- seq(1,5000,1)
data.copy <- data

ggplot(data=data.copy[1:100,],aes(DateTime, Sal, color="red")) 
#  geom_line(data=data[1:100,],aes(DateTime, ODO, color="blue"))


#ggplot() +
#  geom_point(mapping=aes(x=idx, y=Sal), data=data.copy[1:100,]) +
#  geom_point(
#    mapping=aes(x=idx, y=ODO), data=data.copy[1:100,]),
#    colour="red", size=3)



kf1 <- fkf(a0=X0, 
           P0=V0, 
           dt=B%*%g, 
           Tt=A, 
           ct=0, 
           Zt=C, 
           HHt=Sigma1, 
           GGt=Sigma2, 
           yt=matrix(Y,nrow=1))


source("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/11/kalman.R")
head(data)
Kalman.filter <- kalman(data$Sal,A,B=NULL,u=NULL,C,Sigma.1=NULL,Sigma.2=NULL,debug=FALSE,V0=Sigma.1,Xhat0=NULL,n.ahead=1,skip=0,verbose=FALSE)


