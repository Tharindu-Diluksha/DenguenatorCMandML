#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import csv
"""
Created on Sat Nov 11 13:38:14 2017

@author: tharindu
"""


GAMMAH = 0.5*7
SIGMAH = 0.12*7
REPORTING_RATE = 0.04


file_path = "/media/tharindu/626047056046E001/FYP1/Mathematical/data/"
dengue_input_file = file_path+"DataArchive/dengue-modified-withoutlag.csv"
degue_data = pd.read_csv(dengue_input_file)

seir_initial_file = file_path+"SEIR/Initial_SEIR_values_moh69.csv"
seir_data = pd.read_csv(seir_initial_file)
initial_row = seir_data.iloc[0]

moh_area= 69 #MC-COLOMBO
number_of_initial_values = 100 #number of initial values

exposed = []
exposed.append(initial_row.E1) #append the first E value it is same in every initial value

population = degue_data.iloc[469].population
SEIR = [[]for x in range(104)]
for column in seir_data:
    SEIR[0].append(seir_data[column].values[0])
    #print(column ,seir_data[column].values[0])
#SEIR[0].append(initial_row.S1,initial_row.I1,initial_row.R1,initial_row.E1,initial_row.RHO1) 
#print(SEIR)              
for i in range(469,572):#for training weeks 104 , But 104 takes valu from 105(avoid this row for trainnig)
    row = degue_data.iloc[i]
    next_row = degue_data.iloc[i+1]
    exposed.append(next_row.cases/(GAMMAH*REPORTING_RATE))

#for i in range(number_of_initial_values):
#print(SEIR)
#print(exposed)

#print(exposed[103])

for i in range(1,104):
    for j in range(number_of_initial_values):
        S = SEIR[i-1][0+j*5]
        I = SEIR[i-1][1+j*5]
        R = SEIR[i-1][2+j*5]
        E = SEIR[i-1][3+j*5]
        RHO = SEIR[i-1][4+j*5]
        #print(S+I+R+E)
        #print(i)
        
        if((S<=0) or (I<=0) or (R<=0) or (E<=0) or (RHO<=0) or (S+I+R+E <=population-1) or (S+I+R+E >=population+1)):
            SEIR[i].append(-1)
            SEIR[i].append(-1)
            SEIR[i].append(-1)
            SEIR[i].append(-1)
            SEIR[i].append(-1)
            
        else:
            new_S = S-(RHO*S)
            new_I = I+(GAMMAH*exposed[i-1])-(SIGMAH*I)
            new_R = R+(SIGMAH*I)
            new_E = exposed[i]
            if(i+1>=104):
                new_RHO = 0
            else:
                new_RHO = (exposed[i+1]-(1-GAMMAH)*exposed[i])/new_S
            
            SEIR[i].append(new_S)
            SEIR[i].append(new_I)
            SEIR[i].append(new_R)
            SEIR[i].append(new_E)
            SEIR[i].append(new_RHO)
            
#print(SEIR)

headers = []
for i in range(number_of_initial_values):
    headers.append("S"+str(i+1))
    headers.append("I"+str(i+1))
    headers.append("R"+str(i+1))
    headers.append("E"+str(i+1))
    headers.append("RHO"+str(i+1))

with open(file_path+"SEIR/Training_SEIR_RHO_Values_moh_"+ str(moh_area) +".csv", "w") as f:
    writer = csv.writer(f)
    
    writer.writerow(headers)#write header
    for i in range(104):
        new_row_to_write = SEIR[i]
        writer.writerow(new_row_to_write)