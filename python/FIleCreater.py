#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd
#import numpy
import csv
"""
Created on Sun Nov  3 15:38:16 2017

@author: tharindu
"""

file_path = "/media/tharindu/626047056046E001/FYP1/Mathematical/data/"
input_file = file_path+"DataArchive/dengue-modified-withoutlag.csv"
df = pd.read_csv(input_file)

moh_areas = []
for moh in set(df.moh_id):
    if moh not in moh_areas:
        moh_areas.append(moh)
moh_areas.sort()

cases_2012 = [[] for x in range(20)]
cases_2013 = [[] for x in range(20)]
cases_2014 = [[] for x in range(20)]

times = 0
start_index = 0
for moh in moh_areas:
    case12 = 0
    case13 = 0
    case14 = 0
    count = 0
    #for index, row in df.iterrows():
    for i in range(start_index,3120):
        count+=1
        row = df.iloc[i]
        #print(row.moh_name)
        if (count==1):
            cases_2012[times].append(row.moh_id)
            cases_2012[times].append(row.moh_name)
            cases_2013[times].append(row.moh_id)
            cases_2013[times].append(row.moh_name)
            cases_2014[times].append(row.moh_id)
            cases_2014[times].append(row.moh_name)
        if (row.week>=1 and row.week<=52):
            cases_2012[times].append(row.cases)
            case12 += row.cases
        elif (row.week>=53 and row.week<=104):
            cases_2013[times].append(row.cases)
            case13 += row.cases
        elif (row.week>=105 and row.week<=156):
            cases_2014[times].append(row.cases)
            case14 += row.cases
        if (count==156):
            start_index += 156
            cases_2012[times].append(case12)
            cases_2013[times].append(case13)
            cases_2014[times].append(case14)
            break
    times+=1
    
with open(file_path+"dengueCases2012.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerows(cases_2012)
    
with open(file_path+"dengueCases2013.csv", "w") as f1:
    writer = csv.writer(f1)
    writer.writerows(cases_2013)

with open(file_path+"dengueCases2014.csv", "w") as f2:
    writer = csv.writer(f2)
    writer.writerows(cases_2014)
#file_2012 =  numpy.asarray(cases_2012)
#numpy.savetxt(file_path+"dengueCases2012.csv", file_2012, delimiter=",") 
   
print(cases_2012)
print("END")