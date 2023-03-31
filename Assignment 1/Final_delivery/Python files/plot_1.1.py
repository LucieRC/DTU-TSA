## Question 1.1
import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv",sep=" ")

fig,ax = plt.subplots()
ax.plot(data.time.head(718),data.co2.head(718),label="Training set")
ax.plot(data.time.tail(20),data.co2.tail(20),c="red",label="Testing set")
ax.set_xlabel("Time (y)")
ax.set_ylabel("CO2 concentration (ppm)")
ax.legend()