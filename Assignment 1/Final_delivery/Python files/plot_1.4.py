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