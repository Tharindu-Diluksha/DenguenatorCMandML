#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd
import numpy
import csv
from xgboost import XGBRegressor
"""
Created on Sat Nov 11 18:44:25 2017

@author: tharindu
"""

GAMMAH = 0.5*7
REPORTING_RATE = 0.04


file_path = "/media/tharindu/626047056046E001/FYP1/Mathematical/data/"
data_file = file_path+"SEIR/SEIR_Training_moh_69.csv"
mc_colombo = pd.read_csv(data_file)

x_train = mc_colombo.RHO98
y_train = mc_colombo.cases

