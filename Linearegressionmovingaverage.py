import pandas as pd
import matplotlib.pyplot as plt

#from numpy.random import normal
import numpy as np

dateparse = lambda dates: pd.datetime.strptime(dates, '%m/%d/%Y')
df = pd.read_excel('newdata1.xlsx', parse_dates='Day', index_col='Date', date_parser=dateparse)
#df['units'].replace("none","0")
print df
#print df

# get the unique id for each product :
unique_id = df.id.unique()
print len(unique_id)

# slicing the data based on product :
product_list = []
to_forecast_list = []
dates_list = []
for pid in unique_id :
    product = df[ df.id == pid]
    if (len(product) >=8 ):
        product_ts = product.sort_index()
        product_ts = product_ts[:14]
        #print product_ts

        to_forecast = product_ts['units']
        #print to_forecast
        #dates = product_ts['DATE']
        product_list.append(product_ts)
        to_forecast_list.append(to_forecast)
        #dates_list.append(dates)

to_forecast_list = np.asarray(to_forecast_list)
print to_forecast_list
len(product_list)
#print len(product_list)

def mape(ypred, ytrue):
    idx = ytrue != 0.0
    return 100*np.mean(np.abs(ypred[idx]-ytrue[idx])/ytrue[idx])

def mse(ypred, ytrue):
    return (np.square(np.abs(ypred-ytrue))).mean()

def rmse(ypred, ytrue):
    return np.sqrt((np.square(np.abs(ypred-ytrue))).mean())

def organize_data(to_forecast, window, horizon):
    """
     Input:
      to_forecast, univariate time series organized as numpy array
      window, number of items to use in the forecast window
      horizon, horizon of the forecast
     Output:
      X, a matrix where each row contains a forecast window
      y, the target values for each row of X
    """
    shape = to_forecast.shape[:-1] + (to_forecast.shape[-1] - window + 14, window)
    strides = to_forecast.strides + (to_forecast.strides[-1],)
    X = np.lib.stride_tricks.as_strided(to_forecast, 
                                        shape=shape, 
                                        strides=strides)
    #print X
    y = np.array([X[i+horizon][-1] for i in range(len(X)-horizon)])
    #print y
    return X[:-horizon], y


from sklearn.linear_model import LinearRegression

error_list = []
mse_error_list = []
rmse_error_list = []


#    """ returns the mean absolute percentage error """


# do loop : 
for to_forecast in to_forecast_list[::]:
    #print to_forecast.shape



    #for k in range(7,60):

    # number of previous observations to use
    # h = 1
      # forecast horizon
      #X,y = organize_data(to_forecast, k, h)
    h=14
    k=7
    #for k in range(7,60):
    X,y = organize_data(to_forecast, k, h)
    target_matrix = np.column_stack((X,y))
    #print X.shape
    #print y.shape
    #print target_matrix.shape
    m = (target_matrix.shape[0])
    regressor = LinearRegression(normalize=True)
    regressor.fit(X[:m], y[:m])
    error = mape(regressor.predict(X[:m]),y[:m])
    error_list.append(error)
    mse_error = mse(regressor.predict(X[:m]),y[:m])
    rmse_error = rmse(regressor.predict(X[:m]),y[:m])
    rmse_error_list.append(rmse_error)
    mse_error_list.append(mse_error)
    print 'The mse error is %0.4f%', mse_error
    print 'The rmse error is %0.4f%', rmse_error

"""slabel='True demand', color='#377EB8', linewidth=2)
    plt.plot(regressor.predict(X),
    '--', color='#EB3737', linewidth=3, label='Prediction')
    plt.plot(y[:m], label='Train data', color='#3700B8', linewidth=2)
    plt.xticks(range(len(dates))[1::4],dates[1::4], rotation=45)
    plt.legend(loc='upper right')
    plt.ylabel('units_ordered_today')
    plt.show()"""



print 'The MSE for all products is 0.2f',np.asarray(mse_error_list).mean()
print 'The RMSE for all products is 0.2f',np.asarray(rmse_error_list).mean()
    # error graph:
##   error_list = normal(size=100)
#    plt.hist(error_list)
#    plt.title("Error Histogram")
#    plt.xlabel("Product_id")
#    plt.ylabel("Error%")
#    plt.show()
    
 
    
 #loop ends





