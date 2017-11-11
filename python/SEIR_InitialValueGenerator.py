#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import csv
"""
Created on Thu Nov  9 20:56:27 2017

@author: tharindu
"""

GAMMAH = 0.5*7
REPORTING_RATE = 0.04


file_path = "/media/tharindu/626047056046E001/FYP1/Mathematical/data/"
input_file = file_path+"DataArchive/dengue-modified-withoutlag.csv"
df = pd.read_csv(input_file)

moh_area= 69 #MC-COLOMBO
number_of_initial_values = 100

#print(np.random.dirichlet(np.ones(3),size=1))
#random = []
#print(np.random.uniform(1,10,3))
SEIR = [[]for x in range(1)]
count=0

row2 = df.iloc[469]
row3 = df.iloc[470]
E2 = row3.cases//(GAMMAH*REPORTING_RATE)

for x in range(468,469):
    #SEIR[count].append(count+1) not append week number
    row = df.iloc[x]
    exposed = row2.cases/(GAMMAH*REPORTING_RATE)
    SEI_total= row.population-exposed
    for i in range(number_of_initial_values):
        #print(SEI_total)
        randomno = np.random.uniform(1,SEI_total+1,3)
        rand_total = randomno[0]+randomno[1]+randomno[2]
        #print(randomno)
        randomno[0] = randomno[0]*SEI_total/rand_total
        randomno[1] = randomno[1]*SEI_total/rand_total
        randomno[2] = randomno[2]*SEI_total/rand_total
        #print(randomno)
        rand_total = randomno[0]+randomno[1]+randomno[2]
        print(rand_total)
        if(SEI_total!=rand_total):
            minimum_value = min(randomno)
            new_value = minimum_value+SEI_total-rand_total
            for j in range(3):
                if(randomno[j]==minimum_value):
                    randomno[j]=new_value
                    break
        print(randomno,sum(randomno)+exposed)
        for k in range(3):
            SEIR[count].append(randomno[k])
        SEIR[count].append(exposed)
        SEIR[count].append((E2-(1-GAMMAH)*exposed)/randomno[0]) #calculate rho value
    count+=1
    print("======================================")
print(SEIR)

headers = []
for i in range(number_of_initial_values):
    headers.append("S"+str(i+1))
    headers.append("I"+str(i+1))
    headers.append("R"+str(i+1))
    headers.append("E"+str(i+1))
    headers.append("RHO"+str(i+1))

with open(file_path+"SEIR/Initial_SEIR_values_moh"+ str(moh_area) +".csv", "w") as f:
    writer = csv.writer(f)
    #writer.writeheader
    writer.writerow(headers)
    writer.writerows(SEIR)
    #print(count, row.moh_id, row.population, row.cases, Exposed)