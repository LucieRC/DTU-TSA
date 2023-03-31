import matplotlib.pyplot as plt
import pandas as pd
import time
import datetime as dt

data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 3/A3/A3Data.csv")
data = data.rename(columns={"Unnamed: 0":"date"})

fig,ax = plt.subplots(3,1,sharex=True)
ax[0].plot(data["Unnamed: 0"], data["Denmark"], label="House price indicator, Denmark")
ax[1].plot(data["Unnamed: 0"], data["InterestRate"], label="Interest rates")
ax[2].plot(data["Unnamed: 0"], data["InflationRate"], label="Inflation rates")
 
ax[0].legend()
ax[1].legend()
ax[2].legend()



data.date = time.strftime('%Y', data.date)

dt.strptime(data.date, '%Y')
data.date = pd.to_datetime(data.date, format='%y')