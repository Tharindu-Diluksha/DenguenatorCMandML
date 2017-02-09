date = "Sat Jan 28 07:46:58 IST 2017"

#Run all districts at once
results = data.frame()
test = data.frame()
districts = c("Galle")
districts = c("Colombo")
districts = c("Kandy", "Galle")
districts = c("Colombo", "Kandy", "Galle")
districts = districtName_in_colombo[districtName_in_colombo %in% districtNames_population]
districts = districtName_in_kandy[(districtName_in_kandy %in% districtNames_temperature) & 
                       (districtName_in_kandy %in% districtNames_population) &
                       (districtName_in_kandy %in% districtNames_mobility) &
                       (districtName_in_kandy %in% districtNames_dengue12) &
                       (districtName_in_kandy %in% districtNames_dengue13)
                     ]
for (districtName in districts) {
  cat("***************** ",districtName, "   ******************", fill = T)
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/", districtName, sep = '')
  setTrainingAndTest_districts(resultLocation = resultLocation, testLocation = resultLocation, districtName = districtName)
}

results = results[,!(names(results) %in% rankedFeatures[9:29])]
test = test[,!(names(test) %in% rankedFeatures[9:29])]


modelForParamA = trainTheModel(rounds = 3000, depth = 10, learningRate = 0.008)
modelForParamA_districts = trainTheModel(rounds = 1500, depth = 8)

incidencesPlotsCM_districts = list()
imageIndex = 1
for (districtName in districts) {
  cat("***************** ",districtName, "   ******************", fill = T)
  predictSEIR(area = districtName)
  incidencesPlotsCM_districts[[imageIndex]] = plotIncidencesGraphCM_districts(area = districtName)
  imageIndex = imageIndex + 1
}

colNames = colnames(inputs)
importancePlotCM = plotImportanceGraph(featureNames = colNames, model = modelForParamA_districts)

#  Save plots
folderPath = file.path("images", "compartmental model", date)
dir.create(folderPath)
for (districtName in districts) {
  incidencesPlot = incidencesPlotsCM[[grep(districtName, districts)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}

## Run area by area
incidencesPlotsForSeperatedistrictNamesCM_districts = list()
districts = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
districts = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
districts = c("MC - Colombo")
index = 1
for (districtName in districts) {
  results = data.frame()
  test = data.frame()
  
  cat("***************** ",districtName, "   ******************")
  
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/", districtName, sep = '')
  setTrainingAndTest_districts(resultLocation = resultLocation, testLocation = resultLocation, districtName = districtName)
  
  modelForParamA = trainTheModel(depth = 10, rounds = 1000)
  
  testTheModel(area = districtName, model = modelForParamA)
  
  predictSEIR(area = districtName)
  incidencesPlotsForSeperatedistrictNamesCM_districts[[index]] = plotIncidencesGraphCM_districts(area = districtName)
  index = index + 1
}



## Save the last graph
ggsave(filename = "CMC.png", width = 14.23, height = 8, units = "in", dpi = 96)
