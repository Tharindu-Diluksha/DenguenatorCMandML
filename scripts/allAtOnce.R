source("scripts/SEIR_Analysis.R")
source("scripts/xgboost.R")

date = "Tue Jan 24 07:53:11 IST 2017"
date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017" ## CMC RMSE = 21.96, R2 = 0.50
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations ------ RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55
date = "Fri Feb 03 07:01:18 IST 2017" ## All with one iteration, reportingRate=0.2, gammah=3.5, sigmah=0.8400000000000003
date = "Thu Feb 02 19:24:15 IST 2017" ## All with one iteration, reportingRate=0.04, gammah=3.5, sigmah=0.8400000000000003


#Run all areas at once
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Nugegoda", "Kelaniya", "Wattala")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Nugegoda", "Kelaniya", "Wattala")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("Kaduwela", "Maharagama", "MC - Colombo", "Moratuwa")
areas = c("Kaduwela", "Maharagama", "Moratuwa")
areas = c("MC - Colombo")
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
areas = moh_in_kandy[(moh_in_kandy %in% mohs_temperature) & 
               (moh_in_kandy %in% mohs_population) &
               (moh_in_kandy %in% mohs_mobility) &
               (moh_in_kandy %in% mohs_dengue12) &
               (moh_in_kandy %in% mohs_dengue13)
             ]
results = data.frame()
test = data.frame()
for (moh in areas) {
  cat("***************** ",moh, "   ******************", fill = T)
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/", moh, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh, withmobility = F, withcaselags = T)
}

## Tunning the XGB model
R2 <<- 0
bestR2 = 0
bestDepth = 0
bestRounds = 0
for(d in 2:10) {
  for (r in seq(1000, 1600, 200)) {
    modelForParamA = trainTheModel(rounds = r, depth = d, verbose = 0)
    
    incidencesPlotsCM = list()
    imageIndex = 1
    for (moh in areas) {
      cat("***************** ",moh, "   ******************", fill = T)
      predictSEIR(area = moh)
      incidencesPlotsCM[[imageIndex]] = plotIncidencesGraphCM(area = moh)
      imageIndex = imageIndex + 1
    }
    
    if(R2 > bestR2) {
      bestR2 <<- R2
      bestRounds <<- r
      bestDepth <<- d
    }
  }
}

modelForParamA = trainTheModel(rounds = 10, depth = 3, verbose = 1, learningRate = 0.01)

incidencesPlotsCM = list()
imageIndex = 1
for (moh in areas) {
  cat("***************** ",moh, "   ******************", fill = T)
  predictSEIR(area = moh)
  incidencesPlotsCM[[imageIndex]] = plotIncidencesGraphCM(area = moh)
  imageIndex = imageIndex + 1
}
incidencesPlotsCM

paraApred = plotParamA(areas)
# testTheModel(areas, model = modelForParamA)
# plotParamA(areas)

colNames = colnames(inputs)
importancePlotCM = plotImportanceGraph(featureNames = colNames, model = modelForParamA)

incidencesPlotsCM_researpaper = list()
imageIndex = 1
for (moh in areas) {
  cat("***************** ",moh, "   ******************", fill = T)
  predictSEIR(area = moh)
  incidencesPlotsCM_researpaper[[imageIndex]] = plotIncidencesGraphCM2(area = moh)
  imageIndex = imageIndex + 1
}
#  Save plots SVG
folderPath = file.path("images", "compartmental model", "research paper", date)
dir.create(folderPath, recursive = T)
for (moh in areas) {
  incidencesPlot = incidencesPlotsCM[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  ggsave(filename = paste(path, ".svg", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
  
  path = file.path(folderPath, paraApred$labels$title)
  ggsave(filename = paste(path, ".svg", sep = ""), plot = paraApred, width = 14.23, height = 8, units = "in", dpi = 96)
}
#  Save plots JPEG and PNG
for (moh in areas) {
  incidencesPlot = incidencesPlotsCM_researpaper[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".jpeg", sep = ""), plot = incidencesPlot, dpi = 96)
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, dpi = 96)
  
  path = file.path(folderPath, paraApred$labels$title)
  ggsave(filename = paste(path, ".jpeg", sep = ""), plot = paraApred, dpi = 96)
  ggsave(filename = paste(path, ".png", sep = ""), plot = paraApred, dpi = 96)
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


