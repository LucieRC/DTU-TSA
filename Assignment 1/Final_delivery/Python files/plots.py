import os
import pandas as pd 
import matplotlib.pyplot as plt

os.chdir("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignment 1/Final_delivery/Python files")
data = pd.read_csv("../data.csv", sep=" ")
data_without_last_years2 = data[data.time < 2018]

#### Question 1.1
import plot_1.1
#### Question 1.2
import plot_1.2
#### Question 1.3
import plot_1.3
#### Question 1.4
import plot_1.4
