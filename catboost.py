# -*- coding: utf-8 -*-
"""
Created on Sun Oct 13 19:54:32 2019

@author: zhang
"""

import numpy as np
import pandas as pd
import os
#import rpy2.robjects as robjects
#from rpy2.robjects import pandas2ri
#pandas2ri.activate()

#readRDS = robjects.r['readRDS']
os.chdir('D:\download\kaggle\ieee-fraud-detection')

train_df=pd.read_csv('ad1.csv')
test_df=pd.read_csv('ad2.csv')
train_df.head()
#remove na
test_df.replace(['nan', 'None'], -9999)
train_df.replace(['nan', 'None'], -9999)

train_df.fillna(-999, inplace=True)
test_df.fillna(-999, inplace=True)

y = pd.read_csv('label.csv')
from catboost import CatBoostClassifier, Pool, cv
from sklearn.metrics import accuracy_score
categorical_features_indices = np.where(train_df.dtypes != np.float)[0]
model1 = CatBoostClassifier(
    custom_loss=['AUC'],
    random_seed=42,
    logging_level='Silent'
)

model1.fit(
    train_df, y,
    cat_features=categorical_features_indices,
     
     logging_level='Verbose',  # you can uncomment this for text output
    plot=True
)



                                        
