# -*- coding: utf-8 -*-
"""
Created on Tue Sep 26 22:35:41 2017

@author: Xq.Do
Source: https://www.kaggle.com/arjanso/can-parcel-density-predict-the-logerror/notebook
"""

#  Packages
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import gc
from matplotlib import pyplot as plt
from sklearn.neighbors import KDTree
from scipy.stats.stats import pearsonr
from scipy.stats import rankdata


#  Data
train = pd.read_csv('C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/train_2016_v2.csv')
props = pd.read_csv('C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/properties_2016.csv',low_memory=False)
train = train.merge(props, how='left', on='parcelid')
train = train[['parcelid','longitude','latitude','logerror']]
train.dropna(inplace=True)  #
del props  # delete redundant data
gc.collect()  # Free up memory

          
print("DataFrame sample:")
print("***************************************************")
print(train.head())
print("***************************************************")
print("shape = ",train.shape)


#----------------------------------------------------------------#
#               2. Method: Kernel Density Estimation             
#https://en.wikipedia.org/wiki/Kernel_density_estimation#
#----------------------------------------------------------------#


def get_pde(train,bw):
    x = train['longitude'].values
    y = train['latitude'].values
    xy = np.vstack([x,y])
    X = np.transpose(xy)
    tree = KDTree(X,leaf_size = 20 )     
    parcelDensity = tree.kernel_density(X, h=bw,kernel='gaussian',rtol=0.00001)
    return parcelDensity

parcelDensity30000 = get_pde(train,30000)
parcelDensity10000 = get_pde(train,10000)
parcelDensity3000 = get_pde(train,3000)
parcelDensity1000 = get_pde(train,1000)
parcelDensity300 = get_pde(train,300)

rankScaled30000 = 100*rankdata(parcelDensity30000)/len(parcelDensity30000)
rankScaled10000 = 100*rankdata(parcelDensity10000)/len(parcelDensity10000)
rankScaled3000 = 100*rankdata(parcelDensity3000)/len(parcelDensity3000)
rankScaled1000 = 100*rankdata(parcelDensity1000)/len(parcelDensity1000)
rankScaled300 = 100*rankdata(parcelDensity300)/len(parcelDensity300)

out = pd.DataFrame({'longitude' : train['longitude'].values, 
                    'latitude' : train['latitude'].values,
                    'density30000' : parcelDensity30000,
                    'density10000' : parcelDensity10000,
                    'density3000'  : parcelDensity3000,
                    'density1000'  : parcelDensity1000,
                    'density300'   : parcelDensity300})
    
    
out.to_csv('C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price/Excel files/parcel_density.csv')
