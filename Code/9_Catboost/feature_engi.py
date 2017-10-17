# -*- coding: utf-8 -*-
"""
Created on Sat Oct  7 18:48:58 2017

@author: Xq.Do
"""
def feature_engi(df_train):
    #life of property
    df_train['N-life'] = 2018 - df_train['yearbuilt']
    
    #error in calculation of the finished living area of home
    df_train['N-LivingAreaError'] = df_train['calculatedfinishedsquarefeet']/df_train['finishedsquarefeet12']
    
    #proportion of living area
    df_train['N-LivingAreaProp'] = df_train['calculatedfinishedsquarefeet']/df_train['lotsizesquarefeet']
    df_train['N-LivingAreaProp2'] = df_train['finishedsquarefeet12']/df_train['finishedsquarefeet15']
    
    #Amout of extra space
    df_train['N-ExtraSpace'] = df_train['lotsizesquarefeet'] - df_train['calculatedfinishedsquarefeet'] 
    df_train['N-ExtraSpace-2'] = df_train['finishedsquarefeet15'] - df_train['finishedsquarefeet12'] 
    
    #Total number of rooms
    df_train['N-TotalRooms'] = df_train['bathroomcnt']*df_train['bedroomcnt']
    
    #Average room size
    df_train['N-AvRoomSize'] = df_train['calculatedfinishedsquarefeet']/df_train['roomcnt'] 
    
    # Number of Extra rooms
    df_train['N-ExtraRooms'] = df_train['roomcnt'] - df_train['N-TotalRooms'] 
    
    #Ratio of the built structure value to land area
    df_train['N-ValueProp'] = df_train['structuretaxvaluedollarcnt']/df_train['landtaxvaluedollarcnt']
    
    df_train["N-location"] = df_train["latitude"] + df_train["longitude"]
    df_train["N-location-2"] = df_train["latitude"]*df_train["longitude"]
    df_train["N-location-2round"] = df_train["N-location-2"].round(-4)
    
    df_train["N-latitude-round"] = df_train["latitude"].round(-4)
    df_train["N-longitude-round"] = df_train["longitude"].round(-4)
    
    
    
    
    
    #Ratio of tax of property over parcel
    df_train['N-ValueRatio'] = df_train['taxvaluedollarcnt']/df_train['taxamount']
    
    #TotalTaxScore
    df_train['N-TaxScore'] = df_train['taxvaluedollarcnt']*df_train['taxamount']
    
    #polnomials of tax delinquency year
    df_train["N-taxdelinquencyyear-2"] = df_train["taxdelinquencyyear"] ** 2
    df_train["N-taxdelinquencyyear-3"] = df_train["taxdelinquencyyear"] ** 3
    
    #Length of time since unpaid taxes
    df_train['N-life'] = 2018 - df_train['taxdelinquencyyear']
    
    
    
    
    
    #Number of properties in the zip
    zip_count = df_train['regionidzip'].value_counts().to_dict()
    df_train['N-zip_count'] = df_train['regionidzip'].map(zip_count)
    
    #Number of properties in the city
    city_count = df_train['regionidcity'].value_counts().to_dict()
    df_train['N-city_count'] = df_train['regionidcity'].map(city_count)
    
    #Number of properties in the city
    region_count = df_train['regionidcounty'].value_counts().to_dict()
    df_train['N-county_count'] = df_train['regionidcounty'].map(city_count)
    
    
    
    
    #Indicator whether it has AC or not
    df_train['N-ACInd'] = (df_train['airconditioningtypeid']!=5)*1
    
    #Indicator whether it has Heating or not 
    df_train['N-HeatInd'] = (df_train['heatingorsystemtypeid']!=13)*1

    return df_train