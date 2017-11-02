source("scripts/SEIR_Analysis.R")
source("scripts/xgboost.R")

date = "Tue Jan 24 07:53:11 IST 2017"
date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017" ## CMC RMSE = 21.96, R2 = 0.50
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations ------ RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55
date = "Fri Feb 03 07:01:18 IST 2017" ## All with one iteration, reportingRate=0.2, gammah=3.5, sigmah=0.8400000000000003
date = "Thu Feb 02 19:24:15 IST 2017" ## All with one iteration, reportingRate=0.04, gammah=3.5, sigmah=0.8400000000000003
date = "Thu Nov 02 14:37:15 IST 2017"


## Run area by area
incidencesPlotsForSeperateMOHsCM = list()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("MC - Colombo")
index = 1
for (moh in areas) {
  results = data.frame()
  test = data.frame()
  cat("***************** ",moh, "   ******************")
  #resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/",moh,"-", date, sep = '') 
  resultLocation = paste("/media/tharindu/626047056046E001/FYP1/Mathematical/DenguenatorCMandML/results/",moh,"-", date, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
  modelForParamA = trainTheModel(depth = 10)
  testTheModel(area = moh, model = modelForParamA)
  predictSEIR(area = moh)
  incidencesPlotsForSeperateMOHsCM[[index]] = plotIncidencesGraphCM(area = moh)
  index = index + 1
}


