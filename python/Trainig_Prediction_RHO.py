#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd
import numpy
import csv
"""
Created on Sat Nov 11 15:53:54 2017

@author: tharindu
"""

GAMMAH = 0.5*7
REPORTING_RATE = 0.04


file_path = "/media/tharindu/626047056046E001/FYP1/Mathematical/data/"
other_data_file = file_path+"DataArchive/mc-olombo-training-with-one-lag.csv"
mc_colombo = pd.read_csv(other_data_file)

for column in mc_colombo:
    print(column)


SEIR_input_file = file_path+"SEIR/Training_SEIR_RHO_Values_moh_69_T1.csv"
seir_data = pd.read_csv(SEIR_input_file)

moh_area= 69 #MC-COLOMBO
number_of_initial_values = 100
valide_Rho_positions =[]
valid_Rhos = []
count = 0
for column in seir_data:
   #if (column)
    if ("RHO" in column):
        count+=1
        #print(column,seir_data[column].values[102])
        if(seir_data[column].values[102]>0):
            valide_Rho_positions.append(count)
            valid_Rhos.append(seir_data[column].values) 
            #mc_colombo[column] = seir_data[column].values 
            week = []
            for i in range(104):
                week.append(i+1)
            new_column = pd.DataFrame({'week':week ,column: seir_data[column].values})
            mc_colombo = mc_colombo.merge(new_column, left_index = True, right_index = True)
mc_colombo.to_csv(file_path+"SEIR/SEIR_Correlation_Training_moh_"+ str(moh_area) +".csv")
#print(valid_Rhos)
print(len(valid_Rhos),len(valide_Rho_positions))

mc_col_corr = mc_colombo.corr()

(mc_col_corr
     .cases
     .drop(['cases','week_x','week_y','cases_100k','cases_100k_lag_1','cases_lag_1']) # don't compare with myself
     .sort_values(ascending=False)
     .plot
     .barh())



