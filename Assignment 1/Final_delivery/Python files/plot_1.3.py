## Question 1.3
y_est_opti = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/21.01.2023/created_data/y_est_lambda_opti.csv")
data_without_last_years2["y_est_opti"] = y_est_opti.V1
fig3, ax3 = plt.subplots()
ax3.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations",s=1)#, s=1)
ax3.scatter(data_without_last_years2.time, data_without_last_years2.y_est_opti, label="Local trend model",s=1)
ax3.set_xlabel("Time (y)")
ax3.set_ylabel("CO2 concentration (ppm)")
ax3.legend()


epsilon = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/epsilon.all_1.3.3.csv")
sigma_local  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/sigma.local_1.3.3.csv")
data_without_last_years2["sigma_all"] = sigma_local.x
data_without_last_years2["eps"] = epsilon.V1
fig3, ax3 = plt.subplots(2,1,sharex=True,figsize=(12,7))
ax3[0].plot(data_without_last_years2.time, data_without_last_years2.eps, label="1-step predition errors")#, s=1)
ax3[1].plot(data_without_last_years2.time, data_without_last_years2.sigma_all, label="Estimation error")
ax3[0].set_title("One-step prediction errors for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_title("Estimates of sigma for the linear trend model/n of atmospheric CO2 concentration (1959-2018)")
ax3[1].set_xlabel("Time (y)")
ax3[0].set_ylabel("CO2 concentration (ppm)")
ax3[1].set_ylabel("CO2 concentration (ppm)")


y_pred  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/y_est_lambda_0.9.csv")
y_pred_down  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/pred_down_lambda_0.9.csv")
y_pred_up  = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/pred_up_lambda_0.9.csv")
data_without_last_years2["y_pred"] = y_pred.V1
data_without_last_years2["y_pred_down"] = y_pred_down.V1
data_without_last_years2["y_pred_up"] = y_pred_up.V1
fig4, ax4 = plt.subplots(sharex=True)
ax4.scatter(data_without_last_years2.time.head(718).tail(96), data_without_last_years2.co2.head(718).tail(96), label="Observations before 2018", s=1, c="skyblue")
ax4.scatter(data_without_last_years2.time.head(718).tail(96), data_without_last_years2.y_pred.head(718).tail(96), label="Predictions before 2018", s=1, c="tomato")
ax4.scatter(data_without_last_years2.time.tail(20), data_without_last_years2.co2.tail(20), label="Observations after 2018", s=3, c="skyblue")
ax4.scatter(data_without_last_years2.time.tail(20), data_without_last_years2.y_pred.tail(20), label="Predictions after 2018", s=3, c="tomato")
ax4.plot(data_without_last_years2.time.tail(96), data_without_last_years2.y_pred_down.tail(96), label="Lower bound")#, s=1)
ax4.plot(data_without_last_years2.time.tail(96), data_without_last_years2.y_pred_up.tail(96), label="Upper bound")#, s=1)
ax4.set_title("...")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()



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

## Question 1.3.8
y_est_opti = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/y_est_lambda_0.9.csv")
theta = pd.read_csv("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/created_data/theta.all.csv")
data_without_last_years2["y_est_opti"] = y_est_opti.V1
data_without_last_years2["theta"] = theta.V1
fig3, ax3 = plt.subplots()
ax3.scatter(data_without_last_years2.time, data_without_last_years2.co2, label="Observations",s=1)#, s=1)
ax3.plot(data_without_last_years2.time, data_without_last_years2.theta, label="Mean theta_1", c="orange")#,s=1)
ax3.set_xlabel("Time (y)")
ax3.set_ylabel("CO2 concentration (ppm)")
ax3.legend()
