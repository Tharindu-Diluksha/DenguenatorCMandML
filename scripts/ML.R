source("scripts/SEIR_Analysis.R")
source("scripts/xgboost.R")

rm(currentMOH)
trainingDataFrame = data.frame()
testingDataFrame = data.frame()

###################################   ML - Run all at once     #########################
trainingDataFrame = data.frame()
testingDataFrame = data.frame()
incidencesPlotsML = list()
areas = moh_in_galle[moh_in_galle %in% mohs_dengue12]
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
areas = moh_in_kandy[(moh_in_kandy %in% mohs_temperature) & 
                       (moh_in_kandy %in% mohs_population) &
                       (moh_in_kandy %in% mohs_mobility) &
                       (moh_in_kandy %in% mohs_dengue12) &
                       (moh_in_kandy %in% mohs_dengue13)
                     ]
for (mohName in areas) {
  cat("***************** ",mohName, "   ******************", fill = T)
  setTrainingAndTestML(mohName = mohName, withcaselags = T)
}

mlModel = trainTheMLmodel(depth = 6, rounds = 2000)

for (index in 1:length(areas)) {
  area = areas[index]
  cat("***************** ",area, "   ******************", fill = T)
  predictions = testTheMLmodel(area = area, model = mlModel)
  incidencesPlotsML[[index]] = plotIncidencesGraphML(area = area, predictions = predictions)
}

colNames = colnames(trainingDataFrame)
importancePlotML = plotImportanceGraph(featureNames = colNames[!(colNames %in% drops)], model = mlModel)

#  Save plots
date = Sys.time()
folderPath = file.path("images", "ml model", date)
dir.create(folderPath, recursive = T)
for (moh in areas) {
  incidencesPlot = incidencesPlotsML[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}

###################################   ML - Run all at once for seperate MOH areas     #########################
incidencesPlotsForSeperateMOHs = list()
importancePlotsForSeperateMOHs = list()
colNames = colnames(trainingDataFrame)
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
index = 1
for (mohName in areas) {
  trainingDataFrame = data.frame()
  testingDataFrame = data.frame()
  cat("***************** ",mohName, "   ******************")
  setTrainingAndTestML(mohName = mohName)
  
  mlModel = trainTheMLmodel(depth = 10, rounds = 1000)
  
  predictions = testTheMLmodel(area = mohName, model = mlModel)
  incidencesPlotsForSeperateMOHs[[index]] = plotIncidencesGraphML(area = mohName, predictions = predictions)
  
  importancePlotsForSeperateMOHs[[index]] = plotImportanceGraph(featureNames = colNames[!(colNames %in% drops)], model = mlModel)
  importancePlotsForSeperateMOHs[[1]]$labels$subtitle = mohName
  
  index = index + 1
}

#  Save plots
date = Sys.time()
folderPath = file.path("images", "ml model - seperate mohs", date)
dir.create(folderPath)
for (moh in areas) {
  incidencesPlot = incidencesPlotsForSeperateMOHs[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}


############################# Testings  ##################

#([0-9]{4})_([0-9])(_[0-9])?
testRegExp = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")


