library("forecast")
library(car)
library(marima)
install.packages("cowplot")
library(cowplot)
install.packages("dplyr")
library(dplyr)
dev.new()
par("mar")
setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3")
par(mfrow=c(2,1))
library(ggplot2)
source("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3/ggplot.corr.R")


A3Data <- read.csv("A3Data.csv", head=T)
A3Data_regions <- A3Data[1:122,3:6]
A3Data_regions_diff <- apply(A3Data_regions,2,diff)
acf_list <- acf(A3Data_regions_diff, plot=FALSE)
with(acf_list, data.frame(lag, snames, acf))#, acf))
acf_list$lag
attributes(acf_list)
acf_list$snames

# plot(seq(0,20),data.frame(acf(A3Data_regions_diff[,1], plot=FALSE)$acf))
# plot(acf(A3Data_regions_diff[,1], plot=FALSE)$acf,legend("topright",c("my_plot")))
a <- ggplot(data=data.frame(acf(A3Data_regions_diff[,1], plot=FALSE)$acf))#,legend("topright",c("my_plot")))
ggplot(acf(A3Data_regions_diff[,1], plot=FALSE)$acf)
for i in range(2,4):
    # lines(seq(0,20),acf(A3Data_regions_diff[,i], plot=FALSE)$acf)
    lines(acf(A3Data_regions_diff[,i], plot=FALSE)$acf)

ggplot.corr(data=A3Data_regions_diff, lag.max=24, ci= 0.95, large.sample.size=FALSE, horizontal=TRUE)


# conf.level <- 0.95
# ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
bacf <- acf(A3Data_regions_diff[,1], plot=FALSE)
bacf
bacfdf <- with(bacf, data.frame(lag, acf))
bacfdf

library(ggplot2)
q <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
    #    geom_bar(stat="identity", position="identity") + 
       geom_line(x=bacfdf$lag,y=bacfdf$acf)
q

q <- ggplot(data=bacfdf, mapping=aes(x = lag, y = acf)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))

q



var.auto(log)


ln_data_regions <- log(A3Data_regions)
choose_order <- function(i){
    var.auto_i <- ar(ln_data_regions[,i], order.max=10)
    summary(var.auto_i)
    # arima(ln_data_regions[,1], order.max=10)
    return(var.auto_i$aic)
}

var.auto <- ar(ln_data_regions[,1], order.max=10)
summary(var.auto)
# arima(ln_data_regions[,1], order.max=10)
var.auto$aic
# Drop on that
