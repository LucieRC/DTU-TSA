import matplotlib.pyplot as plt
import pandas as pd
import time
import datetime as dt

data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3/A3Data.csv")
data = data.rename(columns={"Unnamed: 0":"date"})

fig,ax = plt.subplots(4,1,sharex=True)
ax[0].plot(data["date"], data["Capital"], label="Capital")
ax[1].plot(data["date"], data["Sealand"], label="Sealand")
ax[2].plot(data["date"], data["MidJutland"], label="MidJutland")
ax[3].plot(data["date"], data["Rural"], label="Rural")

ax[0].legend()
ax[1].legend()
ax[2].legend()
ax[3].legend()
# ax[0].set_xtickslabels(rotation=90)
# ax[1].set_xtickslabels(rotation=90)
# ax[2].set_xtickslabels(rotation=90)
# ax[3].set_xtickslabels(rotation=90)
# ax[3].set_xtickslabels(rotation=90)
# ax[3].set_xticks()
# ax.set_xtickslabels(rotation=90)
plt.setp(ax[3].get_xticklabels(), rotation=45, ha="right",
         rotation_mode="anchor")


# data.date = time.strftime('%Y', data.date)

# dt.strptime(data.date, '%Y')
# data.date = pd.to_datetime(data.date, format='%y')