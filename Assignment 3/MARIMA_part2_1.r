library(marima)
setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3")

data <- read.csv('A3Data.csv')
data_cut <- data[1:119,1:8]
data_cut[1:5,1:8]
data_regions <- data_cut[1:119,1:6]
data_regions[1:5,1:6]

matplot(data_regions, lty=1, type="l", xlab="Time index", ylab="Price index")#, col=rainbow(11))

acf(data_regions[3:6])

tsdiag(data_regions[3:6])
data_regions[3:6]
# acf(data_regions[3:6])

ln_data_regions <- log(data_regions[3:6])
matplot(acf(diff(data_regions,1)))
acf(log(data_regions[3]))
pacf(log(data_regions[3]))
diff(data_regions[3],1)

for (col in colnames(data_regions)) {
}

data_regions

y4<-matrix(round(100*rnorm(4*1000, mean=2.0)), nrow=4)
y4.dif<-define.dif(y4, difference=c(4, 1, 3, 6))
y4.dif.analysis<-y4.dif$y.dif
Mod <- define.model(kvar=4, ar=c(1, 2, 4), ma=c(1), reg.var=3)
arp<-Mod$ar.pattern
map<-Mod$ma.pattern
short.form(arp)
short.form(map)

