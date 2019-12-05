# Stats 506, Fall 2019
# Group Project, Group 2
#
# This script contains functions for interpreting the 2015-2016 NHANES
#
# Author: Sijun Zhang (randyz@umich.edu) (umid:889934761)
# Date: Dec 4, 2019
#80: ---------------------------------------------------------------------------
import numpy as np
import sklearn
import pandas as pd
import glmnet_py
from glmnet import glmnet
import scipy

df_file = "group_2_df.csv"
df = pd.read_csv(df_file)

bpx_sy = df["bpxsy_avg"].values
bpx_diff = df["bpxdiff_avg"].values

# the first element is gender, the next 9 elements are predictors
# the final 9 elements are interaction terms
X = df.iloc[0:,3:22].values

wts = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1]
wts = np.array(wts, ndmin=1)
fit = glmnet(x = X, y = bpx_sy, family = 'gaussian', alpha = 1, nlambda = 20)