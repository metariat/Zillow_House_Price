# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 16:55:34 2017

@author: Xq.Do
"""

import pandas as pd
import numpy as np
from sklearn.linear_model import LassoCV, Lasso 
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV
import gc

properties = pd.read_csv("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_v2.csv")
properties.shape
test = pd.get_dummies(properties)

transactions = pd.read_csv("C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train_2016_v2.csv")

properties['fips'].fillna('6037', inplace = True)

del_col = ["tract.block", "regionidzip", "regionidcity", "censustractandblock", "census", "rawcensustractandblock"]

properties.drop(del_col, axis=1, inplace=True)
properties.shape

data = pd.merge(transactions, properties, how='left', on='parcelid')
data.shape
del_col = [ "transactiondate", "parcelid", 
            "propertyzoningdesc", "regionidneighborhood", "propertycountylandusecode", "calculatedbathnbr.NA.type",
            "taxdelinquencyyear.NA.type",
            "N.life.tax",
            "finishedfloor1squarefeet.NA.type",
            "basementsqft.NA.type",
            "N.tract.count"]
data.drop(del_col, axis = 1, inplace = True)
data.replace([np.inf,-np.inf], 999, inplace = True)


test = pd.get_dummies(data)
test = test[abs(test['logerror']) < 1.5]

X_train = test.drop('logerror', axis = 1)
Y_train = test['logerror']
del properties
gc.collect()
alphas = np.logspace(-4, -0.5, 20)
lasso_cv = LassoCV(alphas=alphas, random_state=0, max_iter = 10000, tol = 0.01)
mod  = lasso_cv.fit(X_train, Y_train)
mod.alpha_


y_hat = mod.predict(X_train)
abs(y_hat-Y_train).mean()

