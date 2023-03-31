import matplotlib.pyplot as pyplot
import pandas as pd


data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")
data_without_last_years2 = data[data.time < 2018]
data_without_last_years2.size
data_without_last_years2.head()

S_lambda  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/S_lambda.csv")
lambd  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/lambda.csv")
# data_without_last_years2["S_lambda"] = S_lambda.x
# data_without_last_years2["lambda"] = lambd.x
# data_without_last_years2["y_pred_up"] = y_pred_up.V1
fig4, ax4 = plt.subplots(sharex=True)
ax4.plot(lambd.x, S_lambda.x)#, s=1) #label="Observations", 
# ax4.scatter(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred.tail(100), label="Predictions", s=1)
# ax4.plot(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred_down.tail(100), label="Lower bound")#, s=1)
# ax4.plot(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred_up.tail(100), label="Upper bound")#, s=1)
ax4.set_title("...")
ax4.set_xlabel("Lambda")
ax4.set_ylabel("SSE")
ax4.legend()