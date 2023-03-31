import pandas as pd 
import matplotlib.pyplot as plt

data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/data.csv", sep=" ")
data_without_last_years2 = data[data.time < 2018]
data_without_last_years2.size
data_without_last_years2.head()

# data_Y_estimated = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/Y_estimated.csv")
# data_Y_estimated.head()
# data_Y_estimated = data_Y_estimated.V1
# data_Y_estimated.head()
# data_Y_estimated.tail()
# data_Y_estimated.size

# fig1, ax1 = plt.subplots()
# ax1.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations", s=1)
# ax1.scatter(data_without_last_years2.time, data_Y_estimated, label="Estimated local linear model", s=1)
# # ax9.scatter(data_without_last_years.time, data_without_last_years.new_model, label="Estimated model OLS", s=1)
# ax1.set_title("Observations and local model for atmospheric CO2 concentration (1959-2018)")
# ax1.set_xlabel("Time (y)")
# ax1.set_ylabel("CO2 concentration (ppm)")
# ax1.legend()
# # fig9.savefig('OLS_WLS.jpg', bbox_inches='tight', dpi=150)

# data_without_last_years2["errors_local_trend"] = data_without_last_years2.co2 - data_Y_estimated
# fig2, ax2 = plt.subplots()
# ax2.scatter(data_without_last_years2.time, data_without_last_years2.errors_local_trend, label="Residuals", s=1)
# ax2.set_title("Residuals of the local linear trend model for atmospheric CO2 concentration (1959-2018)")
# ax2.set_xlabel("Time (y)")
# ax2.set_ylabel("Residuals (ppm)")
# ax2.legend()

sd_theta_all = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/sd.theta.all.csv")
sigma_all  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/sigma.all.csv")
data_without_last_years2["sd_theta_all1"] = sd_theta_all.V1
data_without_last_years2["sd_theta_all2"] = sd_theta_all.V2
data_without_last_years2["sd_theta_all3"] = sd_theta_all.V3
data_without_last_years2["sd_theta_all4"] = sd_theta_all.V4
data_without_last_years2["sigma_all"] = sigma_all.x

fig3, ax3 = plt.subplots(2,1,sharex=True,figsize=(12,7))
ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all1, label="Standard error theta1 (constant)", s=1)
ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all2, label="Standard error theta2 (linear part)", s=1)
ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all3, label="Standard error theta3 (sin part)", s=1)
ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all4, label="Standard error theta4 (cos part)", s=1)
ax3[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax3[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_xlabel("Time (y)")
ax3[0].set_ylabel("CO2 concentration (ppm)")
ax3[1].set_ylabel("CO2 concentration (ppm)")
ax3[0].legend()

y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/Y_estimated.csv")
data_without_last_years2["estimated_local_trend"] = y_pred.V1

fig4, ax4 = plt.subplots(3,1,sharex=True,figsize=(12,14))
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all1, label="Standard error theta1 (constant)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all2, label="Standard error theta2 (linear part)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all3, label="Standard error theta3 (sin part)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all4, label="Standard error theta4 (cos part)", s=1)
ax4[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax4[2].scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations", s=1)
ax4[2].scatter(data_without_last_years2.time, data_without_last_years2.estimated_local_trend, label="Local trend model", s=1)

ax4[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax4[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax4[2].set_title("Observations and local trend model estimates/n of atmospheric CO2 concentration (1959-2018)")

ax4[1].set_xlabel("Time (y)")
ax4[0].set_ylabel("CO2 concentration (ppm)")
ax4[1].set_ylabel("CO2 concentration (ppm)")
ax4[2].set_ylabel("CO2 concentration (ppm)")
ax4[0].legend()
ax4[2].legend()



matrix_y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_local_trend.csv")
data_without_last_years2["y_pred"] = matrix_y_pred.V1
data_without_last_years2["y_pred_lower"] = matrix_y_pred.V2
data_without_last_years2["y_pred_upper"] = matrix_y_pred.V3
fig4, ax4 = plt.subplots(3,1,sharex=True,figsize=(12,14))
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all1, label="Standard error theta1 (constant)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all2, label="Standard error theta2 (linear part)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all3, label="Standard error theta3 (sin part)", s=1)
ax4[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all4, label="Standard error theta4 (cos part)", s=1)
ax4[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax4[2].scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations", s=1)
ax4[2].scatter(data_without_last_years2.time, data_without_last_years2.y_pred, label="Local trend model", s=1)
# ax4[2].plot(data_without_last_years2.time, data_without_last_years2.y_pred_lower, label="Lower bound") #, s=1)
# ax4[2].plot(data_without_last_years2.time, data_without_last_years2.y_pred_upper, label="Upper bound") #, s=1)

ax4[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax4[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax4[2].set_title("Observations and local trend model estimates/n of atmospheric CO2 concentration (1959-2018)")

ax4[1].set_xlabel("Time (y)")
ax4[0].set_ylabel("CO2 concentration (ppm)")
ax4[1].set_ylabel("CO2 concentration (ppm)")
ax4[2].set_ylabel("CO2 concentration (ppm)")
ax4[0].legend()
ax4[2].legend()






# matrix_y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_local_trend.csv")
# data_without_last_years2["y_pred"] = matrix_y_pred.V1
fig4, ax4 = plt.subplots()
ax4.plot(data_without_last_years2.time.tail(1000), data_without_last_years2.co2.tail(1000), label="Observations")#, s=1)
ax4.plot(data_without_last_years2.time.tail(1000), data_without_last_years2.y_pred.tail(1000), label="Local trend model")#, s=1)
ax4.set_title("Observations and local trend model estimates/n of atmospheric CO2 concentration (1959-2018)")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()


matrix_y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Assignement 1/y_pred_lect4.csv")
data_without_last_years2["from_book"] = matrix_y_pred.x
fig4, ax4 = plt.subplots()
ax4.scatter(data_without_last_years2.time.tail(100), data_without_last_years2.co2.tail(100), label="Observations", s=1)
ax4.scatter(data_without_last_years2.time.tail(100), data_without_last_years2.from_book.tail(100), label="Local trend model", s=1)
# ax4.set_title("Observations and local trend model estimates/n of atmospheric CO2 concentration (1959-2018)")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()


matrix_y_pred = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.12.2023/y_pred_local_trend.csv")
data_without_last_years2["from_book"] = matrix_y_pred.V1
fig4, ax4 = plt.subplots()
ax4.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations", s=1)
ax4.scatter(data_without_last_years2.time, data_without_last_years2.from_book, label="Local trend model", s=1)
# ax4.set_title("Observations and local trend model estimates/n of atmospheric CO2 concentration (1959-2018)")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()





epsilon = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/epsilon.all.csv")
sd_theta_all = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/sd.theta.all.csv")
sigma_all  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/sigma.all.csv")
data_without_last_years2["sd_theta_all1"] = sd_theta_all.V1
data_without_last_years2["sd_theta_all2"] = sd_theta_all.V2
data_without_last_years2["sd_theta_all3"] = sd_theta_all.V3
data_without_last_years2["sd_theta_all4"] = sd_theta_all.V4
data_without_last_years2["sigma_all"] = sigma_all.x
data_without_last_years2["eps"] = epsilon.V1

fig3, ax3 = plt.subplots(2,1,sharex=True,figsize=(12,7))
# ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all1, label="Standard error theta1 (constant)", s=1)
# ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all2, label="Standard error theta2 (linear part)", s=1)
# ax3[0].scatter(data_without_last_years2.time, data_without_last_years2.sd_theta_all3, label="Standard error theta3 (sin part)", s=1)
ax3[0].plot(data_without_last_years2.time, data_without_last_years2.eps, label="1-step predition errors")#, s=1)
ax3[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax3[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_xlabel("Time (y)")
ax3[0].set_ylabel("CO2 concentration (ppm)")
ax3[1].set_ylabel("CO2 concentration (ppm)")
# ax3[0].legend()


martin_data = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/from_martin.csv")
data_without_last_years2["martin"] = martin_data.V1
fig3, ax3 = plt.subplots(sharex=True,figsize=(12,7))
ax3.scatter(data_without_last_years2.time, data_without_last_years2.martin, label="Local trend model", s=1)
ax3.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations",s=1)#, s=1)
# ax3.plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
# ax3[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
# ax3[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3.set_xlabel("Time (y)")
ax3.set_ylabel("CO2 concentration (ppm)")
# ax3[1].set_ylabel("CO2 concentration (ppm)")






y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_local_trend.csv")
y_pred_down  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_down.csv")
y_pred_up  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_up.csv")
data_without_last_years2["y_pred"] = y_pred.V1
data_without_last_years2["y_pred_down"] = y_pred_down.V1
data_without_last_years2["y_pred_up"] = y_pred_up.V1
fig4, ax4 = plt.subplots(sharex=True)
ax4.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations", s=.5)
ax4.scatter(data_without_last_years2.time, data_without_last_years2.y_pred, label="Predictions", s=.5)
# ax4.plot(data_without_last_years2.time, data_without_last_years2.y_pred_up, label="Upper bound prediction interval", alpha=0.5)#, s=1)
# ax4.plot(data_without_last_years2.time, data_without_last_years2.y_pred_down, label="Lower bound prediction interval", alpha=0.5)#, s=1)
# ax4.set_title("...")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()


y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_local_trend.csv")
y_pred_down  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_down.csv")
y_pred_up  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/y_pred_up.csv")
data_without_last_years2["y_pred"] = y_pred.V1
data_without_last_years2["y_pred_down"] = y_pred_down.V1
data_without_last_years2["y_pred_up"] = y_pred_up.V1
fig4, ax4 = plt.subplots(sharex=True,figsize=(12,14))
ax4.scatter(data_without_last_years2.time.tail(100), data_without_last_years2.co2.tail(100), label="Observations", s=1)
ax4.scatter(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred.tail(100), label="Predictions", s=1)
ax4.plot(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred_down.tail(100), label="Lower bound")#, s=1)
ax4.plot(data_without_last_years2.time.tail(100), data_without_last_years2.y_pred_up.tail(100), label="Upper bound")#, s=1)
ax4.set_title("...")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()


## Question 1.3.5
y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_est_lambda.csv")
y_pred_down  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_down.csv")
y_pred_up  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_up.csv")
data["y_pred"] = y_pred.x
data["y_pred_down"] = y_pred_down.x
data["y_pred_up"] = y_pred_up.x
fig4, ax4 = plt.subplots(sharex=True)
ax4.scatter(data.time.tail(96), data.co2.tail(96), label="Observations", s=1)
ax4.scatter(data.time.tail(96), data.y_pred.tail(96), label="Predictions", s=1)
ax4.plot(data.time.tail(96), data.y_pred_down.tail(96), label="Lower bound", alpha=.5)#, s=1)
ax4.plot(data.time.tail(96), data.y_pred_up.tail(96), label="Upper bound", alpha=.5)#, s=1)
ax4.scatter(data.time[718+1-1],408.2123,s=15,label="Prediction 1 month ahead")
ax4.scatter(data.time[718+2-1],410.0793,s=15,label="Prediction 2 months ahead")
ax4.scatter(data.time[718+6-1],410.8324,s=15, label="Prediction 6 months ahead")
ax4.scatter(data.time[718+12-1],408.9988,s=15,label="Prediction 12 months ahead")
ax4.scatter(data.time[718+20-1],410.6437,s=15,label="Prediction 20 months ahead")
# ax4.set_title("...")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()






epsilon = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/epsilon.all.csv")
sd_theta_all = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/sd.theta.all.csv")
sigma_all  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/sigma.all.csv")
# data_without_last_years2["sd_theta_all1"] = sd_theta_all.V1
# data_without_last_years2["sd_theta_all2"] = sd_theta_all.V2
# data_without_last_years2["sd_theta_all3"] = sd_theta_all.V3
# data_without_last_years2["sd_theta_all4"] = sd_theta_all.V4
data_without_last_years2["sigma_all"] = sigma_all.x
data_without_last_years2["eps"] = epsilon.x

fig3, ax3 = plt.subplots(2,1,sharex=True,figsize=(12,7))
ax3[0].plot(data_without_last_years2.time, data_without_last_years2.eps, label="1-step predition errors")#, s=1)
ax3[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax3[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_xlabel("Time (y)")
ax3[0].set_ylabel("CO2 concentration (ppm)")
ax3[1].set_ylabel("CO2 concentration (ppm)")
# ax3[0].legend()





y_est_opti = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_est_lambda_opti.csv")
y_est_opti_down = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_down.csv")
y_est_opti_up = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_pred_up.csv")
data_without_last_years2["y_est_opti"] = y_est_opti.V1
data_without_last_years2["y_est_opti_down"] = y_est_opti_down.V1
data_without_last_years2["y_est_opti_up"] = y_est_opti_up.V1
fig3, ax3 = plt.subplots()
ax3.scatter(data_without_last_years2.time.tail(96), data_without_last_years2.co2.tail(96), label="Observations",s=1)#, s=1)
ax3.scatter(data_without_last_years2.time.tail(96), data_without_last_years2.y_est_opti.tail(96), label="Local trend model",s=1)
ax3.plot(data_without_last_years2.time.tail(96), data_without_last_years2.y_est_opti_down.tail(96), label="Lower bound prediction interval",alpha=.5)
ax3.plot(data_without_last_years2.time.tail(96), data_without_last_years2.y_est_opti_up.tail(96), label="Upper bound prediction interval",alpha=.5)
ax3.set_xlabel("Time (y)")
ax3.set_ylabel("CO2 concentration (ppm)")
ax3.legend()