# -*- coding: utf-8 -*-
"""
Created on Sat Oct  7 10:25:53 2017

@author: Xq.Do
"""

import pandas as pd
import numpy as np
from catboost import CatBoostRegressor
from tqdm import tqdm

""" 
----------------------------------------------------------------
                     Data loading 
----------------------------------------------------------------
""" 

path = "C:/documents/xq.do/Desktop/Kaggle/Zillow_House_Price_Data/"

train_2016 = pd.read_csv(path + 'train_2016_v2.csv', parse_dates=['transactiondate'], low_memory=False)
train_2017 = pd.read_csv(path + 'train_2017.csv', parse_dates=['transactiondate'], low_memory=False)
train_df = pd.concat([train_2016, train_2017])
test_df = pd.read_csv(path + 'sample_submission.csv', low_memory=False)
properties = pd.read_csv(path + 'properties_2017.csv', low_memory=False)
water = pd.read_csv(path + 'water_distance.csv')
density = pd.read_csv(path + 'parcel_density.csv')
del density['latitude']
del density['longitude']
del density['Unnamed: 0']

properties = properties.merge(water, how = 'left', left_on = 'parcelid', right_on = 'id.parcel')
del properties['id.parcel']
properties = properties.merge(density, how = 'left', on = 'parcelid')
test_df['parcelid'] = test_df['ParcelId']


# similar to the1owl
def add_date_features(df):
    df["transaction_year"] = df["transactiondate"].dt.year
    df["transaction_month"] = df["transactiondate"].dt.month
    df["transaction_day"] = df["transactiondate"].dt.day
    df["transaction_quarter"] = df["transactiondate"].dt.quarter
    df.drop(["transactiondate"], inplace=True, axis=1)
    return df
	
	
	
train_df = add_date_features(train_df)
train_df = train_df.merge(properties, how='left', on='parcelid')
test_df = test_df.merge(properties, how='left', on='parcelid')
print("Train: ", train_df.shape)
print("Test: ", test_df.shape)	



missing_perc_thresh = 0.98
exclude_missing = []
num_rows = train_df.shape[0]
for c in train_df.columns:
    num_missing = train_df[c].isnull().sum()
    if num_missing == 0:
        continue
    missing_frac = num_missing / float(num_rows)
    if missing_frac > missing_perc_thresh:
        exclude_missing.append(c)
print("We exclude: %s" % exclude_missing)
print(len(exclude_missing))


# exclude where we only have one unique value :D
exclude_unique = []
for c in train_df.columns:
    num_uniques = len(train_df[c].unique())
    if train_df[c].isnull().sum() != 0:
        num_uniques -= 1
    if num_uniques == 1:
        exclude_unique.append(c)
print("We exclude: %s" % exclude_unique)
print(len(exclude_unique))



exclude_other = ['parcelid', 'logerror']  # for indexing/training only
# do not know what this is LARS, 'SHCG' 'COR2YY' 'LNR2RPD-R3' ?!?
exclude_other.append('propertyzoningdesc')
train_features = []
for c in train_df.columns:
    if c not in exclude_missing \
       and c not in exclude_other and c not in exclude_unique:
        train_features.append(c)
print("We use these for training: %s" % train_features)
print(len(train_features))


cat_feature_inds = []
cat_unique_thresh = 1000
for i, c in enumerate(train_features):
    num_uniques = len(train_df[c].unique())
    if num_uniques < cat_unique_thresh \
       and not 'sqft' in c \
       and not 'cnt' in c \
       and not 'nbr' in c \
       and not 'number' in c:
        cat_feature_inds.append(i)
        
print("Cat features are: %s" % [train_features[ind] for ind in cat_feature_inds])



# some out of range int is a good choice
train_df.fillna(-999, inplace=True)
test_df.fillna(-999, inplace=True)

train_df = feature_engi(train_df)

train_features = []
for c in train_df.columns:
    if c not in exclude_missing \
       and c not in exclude_other and c not in exclude_unique:
        train_features.append(c)
print("We use these for training: %s" % train_features)
print(len(train_features))





from sklearn.model_selection import train_test_split

train, test = train_test_split(train_df, train_size=0.8, random_state=1)


X_train = train[train_features]
y_train = train.logerror
print(X_train.shape, y_train.shape)

X_test = test[train_features]
y_test = test.logerror
print(X_test.shape, y_test.shape)



model = CatBoostRegressor(
        iterations=1000, learning_rate=0.03,
        depth=6, l2_leaf_reg=3,
        loss_function='MAE',
        eval_metric='MAE',
        random_seed=5)


model.fit(
    X_train, y_train,
    cat_features=cat_feature_inds,
    eval_set=(X_test, y_test),
    verbose=True
)

"""
bestTest = 0.06922281272
bestIteration = 776
597
455
636
"""

X = train_df[train_features]
y = train_df.logerror

from catboost import Pool, cv


cv_data = cv(
    model.get_params(),
    Pool(X, label=y, cat_features=cat_feature_inds),
)

save = cv_data

print('Best validation accuracy score: {:.2f}±{:.2f} on step {}'.format(
    np.max(cv_data['Accuracy_test_avg']),
    cv_data['Accuracy_test_stddev'][np.argmax(cv_data['Accuracy_test_avg'])],
    np.argmax(save["b'MAE'_test_avg"])
))

cv_data