import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
from math import pi, sin, cos


data = pd.read_csv("data.csv", sep=" ")
type(data)

# fig1, ax = plt.subplots()
# ax.scatter(data.time, data.co2, s=1)
# ax.set_title("Atmospheric CO2 concentration (1959-2017)")
# ax.set_xlabel("Time (y)")
# ax.set_ylabel("CO2 concentration (ppm)")

data_without_last_years = data[data.year.isin(range(1959,2018))]
fig2, ax2 = plt.subplots()
ax2.plot(data_without_last_years.year, data_without_last_years.co2)
ax2.set_title("Atmospheric CO2 concentration (1959-2017)")
ax2.set_xlabel("Time (y)")
ax2.set_ylabel("CO2 concentration (ppm)")

def model1(X, alpha, beta_t, beta_s, beta_c, p):
    Y = alpha + beta_t*X + beta_s*np.sin(2*np.pi/p*X)+ beta_c*np.cos(2*np.pi/p*X)
    return Y

# def linear_model1(X, alpha, beta_t):
#     Y = alpha + beta_t*X
#     return Y

# def harmonic_model(X, beta_s, beta_c, p):
#     Y = beta_s*np.sin(2*np.pi/p*X)+ beta_c*np.cos(2*np.pi/p*X)
#     return Y

length_series = data_without_last_years["time"].size
X_train = np.hstack((np.ones((length_series,1)),np.array(data_without_last_years["time"]).reshape(length_series,1)))
Y_train = np.array(data_without_last_years["co2"]).reshape(length_series,1)

params, cov = curve_fit(model1, X_train, Y_train)

# params_linear, cov_linear = curve_fit(linear_model1, X_train, Y_train)



# X_train2 = np.array(data_without_last_years["time"])
# Y_train2 = np.array(data_without_last_years["co2"])
# params_linear2, cov_linear2 = curve_fit(linear_model1, X_train2, Y_train2)




# fig3, ax3 = plt.subplots()
# ax3.plot(data_without_last_years.year, data_without_last_years.co2, label="Observations")
# ax3.plot(data_without_last_years.year, params_linear2[0] + params_linear2[1]*data_without_last_years.time, label="Linear model")
# ax3.set_title("Atmospheric CO2 concentration (1959-2017)")
# ax3.set_xlabel("Time (y)")
# ax3.set_ylabel("CO2 concentration (ppm)")
# ax3.legend()

# def create_linear(year_col):
#     linear_col = params_linear2[0] + params_linear2[1]*year_col
#     return linear_col

# data_without_last_years["linear"] = data_without_last_years["time"].apply(create_linear)
# data_without_last_years["co2_without_linear"] = data_without_last_years["co2"] - data_without_last_years["linear"]


# fig4, ax4 = plt.subplots()
# ax4.plot(data_without_last_years.year, data_without_last_years.co2_without_linear, label="Observations")
# ax4.set_title("Atmospheric CO2 concentration without the linear trend (1959-2017)")
# ax4.set_xlabel("Time (y)")
# ax4.set_ylabel("CO2 concentration (ppm)")
# ax4.legend()



# X_train3 = np.array(data_without_last_years["time"])
# Y_train3 = np.array(data_without_last_years["co2_without_linear"])
# params_harmonic3, cov_harmonic3 = curve_fit(harmonic_model, X_train3, Y_train3)


#Check the total model:
# def total_model(X, alpha, beta_t, beta_s, beta_c, p):
#     Y = alpha + beta_t*X + beta_s*np.sin(2*np.pi/p*X)+ beta_c*np.cos(2*np.pi/p*X)
#     return Y

def total_model_est(time, alpha, beta_t, beta_s, beta_c, p):
    Y = alpha + beta_t*time + beta_s*sin(2*pi/p*time)+ beta_c*cos(2*pi/p*time)
    return Y

def compute_estimation(time_scalar):
    estim = total_model_est(time_scalar, params[0], params[1], params[2], params[3], params[4])
    return estim


data_without_last_years["estimated_co2"] = data_without_last_years["time"].apply(compute_estimation)


fig4, ax4 = plt.subplots()
ax4.plot(data_without_last_years.year, data_without_last_years.co2, label="Observations")
ax4.plot(data_without_last_years.year, data_without_last_years.estimated_co2, label="Model")
ax4.set_title("Observations and model results for atmospheric CO2 concentration (1959-2017)")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()