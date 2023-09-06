from statsmodels.tsa.statespace.sarimax import SARIMAX
import numpy as np

import warnings
from statsmodels.tools.sm_exceptions import ConvergenceWarning
warnings.simplefilter('ignore', ConvergenceWarning)

def check_model(data, orders):

    print(orders)

    model = SARIMAX(data,
                    order= orders[0],
                    seasonal_order= orders[1],
                    enforce_invertibility= False,
                    enforce_stationarity= False)

    res = model.fit(disp= False)

    return [orders, res.aic, res.bic, res.hqic, np.sum(res.resid)]