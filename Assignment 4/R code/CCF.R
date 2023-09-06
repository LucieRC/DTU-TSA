# Set working directory:
setwd('/Users/emilhaugstvedt/Desktop/DTU/TimeSeriesAnalysis/Time-Series-Analysis/Assignment4/R code')

# Load data:
data <- read.csv("../A4_data.csv")

# Training data:
training_data <- data[1:114, ]

z2000 <- training_data$z2000
z2800 <- training_data$z2800
z4000 <- training_data$z4000
z4200 <- training_data$z4200

# Calculate and plot ccf:
par(mfrow=c(3,2),mar=c(2,2,3,2))
ccf(z2000,z2800)
ccf(z2000,z4000)
ccf(z2000,z4200)
ccf(z2800,z4000)
ccf(z2800,z4200)
ccf(z4000,z4200)
