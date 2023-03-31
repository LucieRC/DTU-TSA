import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
from math import pi, sin, cos

## Question 1.2.1
data = pd.read_csv("../data.csv", sep=" ")
data_without_last_years = data[data.year<2018]
length_series = data_without_last_years["time"].size
X_train = np.hstack((np.ones((length_series,1)),np.array(data_without_last_years["time"]).reshape(length_series,1)))
Y_train = np.array(data_without_last_years["co2"]).reshape(length_series,1)

def model1(X, alpha, beta_t, beta_s, beta_c, p):
    Y = alpha + beta_t*X + beta_s*np.sin(2*np.pi/p*X)+ beta_c*np.cos(2*np.pi/p*X)
    return Y
params, cov = curve_fit(model1, X_train, Y_train)

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