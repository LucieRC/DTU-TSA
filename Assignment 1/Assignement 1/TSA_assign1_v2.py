import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
from math import pi, sin, cos


data = pd.read_csv("data.csv", sep=" ")
type(data)

data_without_last_years = data[data.year.isin(range(1959,2018))]
data_without_last_years2 = data[data.time < 2018]

def linear_and_harmonic_model(X, alpha, beta_t, beta_s, beta_c, p):
    Y = alpha + beta_t*X + beta_s*np.sin(2*np.pi/p*X)+ beta_c*np.cos(2*np.pi/p*X)
    return Y

X_train2 = np.array(data_without_last_years["time"])
Y_train2 = np.array(data_without_last_years["co2"])
params_linear2, cov_linear2 = curve_fit(linear_and_harmonic_model, X_train2, Y_train2)


def total_model_est(time, alpha, beta_t, beta_s, beta_c, p):
    Y = alpha + beta_t*time + beta_s*sin(2*pi/p*time)+ beta_c*cos(2*pi/p*time)
    return Y

def compute_estimation(time_scalar):
    estim = total_model_est(time_scalar, params_linear2[0], params_linear2[1], params_linear2[2], params_linear2[3], params_linear2[4])
    return estim

data_without_last_years["new_model"] = data_without_last_years.time.apply(compute_estimation)

fig4, ax4 = plt.subplots()
ax4.scatter(data_without_last_years.time, data_without_last_years.co2, label="Observations", s=1)
ax4.scatter(data_without_last_years.time, data_without_last_years.new_model, label="Model", s=1)
ax4.set_title("Observations and model results for atmospheric CO2 concentration (1959-2017)")
ax4.set_xlabel("Time (y)")
ax4.set_ylabel("CO2 concentration (ppm)")
ax4.legend()
fig4.savefig('obversations_model_till_2018.jpg', bbox_inches='tight', dpi=150)



# Plot with the predicted part
data["new_model"] = data.time.apply(compute_estimation)

fig5, ax5 = plt.subplots()
ax5.scatter(data.time, data.co2, label="Observations", s=1)
ax5.scatter(data.time[:-20], data.new_model[:-20], label="Model before 2018", s=1)
ax5.scatter(data.time[-20:], data.new_model[-20:], label="Model after 2018", s=1)
ax5.set_title("Observations and model results for atmospheric CO2 concentration (1959-2020)")
ax5.set_xlabel("Time (y)")
ax5.set_ylabel("CO2 concentration (ppm)")
ax5.legend()
fig4.savefig('before_after_2018.jpg', bbox_inches='tight', dpi=150)


# Compute an estimation of the uncertainty:
data_without_last_years["co2"] - data_without_last_years["new_model"]


# Plot the residuals:
data_without_last_years["residuals"] = data_without_last_years["co2"] - data_without_last_years["new_model"]

fig6, ax6 = plt.subplots()
ax6.scatter(data_without_last_years.time, data_without_last_years.residuals, label="Residuals before 2018", s=1)
ax6.set_title("Residuals of our model for atmospheric CO2 concentration (1959-2018)")
ax6.set_xlabel("Time (y)")
ax6.set_ylabel("Residuals (ppm)")
ax6.legend()
fig6.savefig('residuals_before_2018.jpg', bbox_inches='tight', dpi=150)


data_without_last_years["From_WLS"] = linear_and_harmonic_model(X_train2, -2775.411, 1.574234, 2.645471, -1.019878, 1)
fig7, ax7 = plt.subplots()
ax7.scatter(data_without_last_years.time, data_without_last_years.From_WLS, label="Estimated model", s=1)
ax7.scatter(data_without_last_years.time, data_without_last_years.co2, label="Observations", s=1)
ax7.set_title("Observations and WLS model for atmospheric CO2 concentration (1959-2018)")
ax7.set_xlabel("Time (y)")
ax7.set_ylabel("Residuals (ppm)")
ax7.legend()
fig7.savefig('WLS_model_2018.jpg', bbox_inches='tight', dpi=150)

data_without_last_years["residuals_WLS"] = data_without_last_years["co2"] - data_without_last_years["From_WLS"]
fig8, ax8 = plt.subplots()
ax8.scatter(data_without_last_years.time, data_without_last_years.residuals_WLS, s=1) #label="Residuals before 2018"
ax8.set_title("Residuals of the WLS model for atmospheric CO2 concentration (1959-2018)")
ax8.set_xlabel("Time (y)")
ax8.set_ylabel("Residuals (ppm)")
ax8.legend()
fig8.savefig('residuals_WLS_before_2018.jpg', bbox_inches='tight', dpi=150)


data_without_last_years["From_WLS"] = linear_and_harmonic_model(X_train2, -2775.411, 1.574234, 2.645471, -1.019878, 1)
fig9, ax9 = plt.subplots()
ax9.scatter(data_without_last_years.time, data_without_last_years.From_WLS, label="Estimated model WLS", s=1)
ax9.scatter(data_without_last_years.time, data_without_last_years.co2, label="Observations", s=1)
ax9.scatter(data_without_last_years.time, data_without_last_years.new_model, label="Estimated model OLS", s=1)
ax9.set_title("Observations and models for atmospheric CO2 concentration (1959-2018)")
ax9.set_xlabel("Time (y)")
ax9.set_ylabel("Residuals (ppm)")
ax9.legend()
fig9.savefig('OLS_WLS.jpg', bbox_inches='tight', dpi=150)


# data_without_last_years["residuals"] = data_without_last_years["co2"] - data_without_last_years["new_model"]
# fig6, ax6 = plt.subplots()
# ax6.scatter(data_without_last_years.time, data_without_last_years.residuals, label="Residuals before 2018", s=1)
# ax6.set_title("Residuals of our model for atmospheric CO2 concentration (1959-2018)")
# ax6.set_xlabel("Time (y)")
# ax6.set_ylabel("Residuals (ppm)")
# ax6.legend()