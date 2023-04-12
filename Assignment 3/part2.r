setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3")

data <- read.csv('A3Data.csv')
data_cut <- data[1:119,1:8]
data_cut 

plot(data_cut$X, data_cut$Denmark)
# , type="l", xlab="Year",
#      ylab="Rate of armed suicides", main="Prediction of suicides by firearms",
#      ylim=c(0.0,4.1))

marima(y4.dif.analysis, ar.pattern=arp, ma.pattern=map, 
                penalty=0.0)