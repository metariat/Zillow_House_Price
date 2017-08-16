# Zillow_House_Price repo
The pipeline of the workflow can be found at:

https://github.com/metariat/Zillow_House_Price/blob/master/Presentation/Kaggle%20Zillow%20Price.pptx

Some remarks before the modelisation:
  - Check the sknewness of the target variable: 1-> log-transform it
  - Check the dictionary, note that perhaps for some variable, NA means something.
  - Check if some numerical variables are actually categorical
  - Check the ordinal categorical vs non-ordinal categorical variables.
  - Check the area related variable for feature engineering
  - Apply Box-Cox transformation to highly skewed variables: http://onlinestatbook.com/2/transformations/box-cox.html
  - Dummifying the categorical variables (check the number of levels before)
