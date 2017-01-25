source("scripts/SEIR_Analysis.R")
source("scripts/xgboost.R")

date = "Tue Jan 24 07:53:11 IST 2017"
date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017" ## CMC RMSE = 21.96, R2 = 0.50
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations ------ RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55


#Run all areas at once
results = data.frame()
test = data.frame()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Nugegoda", "Kelaniya", "Wattala")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("Kaduwela", "Maharagama", "MC - Colombo", "Moratuwa")
areas = c("MC - Colombo")
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
areas = moh_in_kandy[(moh_in_kandy %in% mohs_temperature) & 
               (moh_in_kandy %in% mohs_population) &
               (moh_in_kandy %in% mohs_mobility) &
               (moh_in_kandy %in% mohs_dengue12) &
               (moh_in_kandy %in% mohs_dengue13)
             ]
for (moh in areas) {
  cat("***************** ",moh, "   ******************", fill = T)
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/", moh, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
}

modelForParamA = trainTheModel(rounds = 1500, depth = 10)

incidencesPlotsCM = list()
imageIndex = 1
for (moh in areas) {
  predictSEIR(area = moh)
  incidencesPlotsCM[[imageIndex]] = plotIncidencesGraphCM(area = moh)
  imageIndex = imageIndex + 1
}

colNames = colnames(inputs)
importancePlotCM = plotImportanceGraph(featureNames = colNames, model = modelForParamA)

#  Save plots
folderPath = file.path("images", "compartmental model", date)
dir.create(folderPath)
for (moh in areas) {
  incidencesPlot = incidencesPlotsCM[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}

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
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/",moh,"-", date, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
  modelForParamA = trainTheModel(depth = 10)
  testTheModel(area = moh, model = modelForParamA)
  predictSEIR(area = moh)
  incidencesPlotsForSeperateMOHsCM[[index]] = plotIncidencesGraphCM(area = moh)
  index = index + 1
}



## Save the last graph
ggsave(filename = "CMC.png", width = 14.23, height = 8, units = "in", dpi = 96)
