source("scripts/SEIR_Analysis.R")

## Run area by area
incidencesPlotsForSeperateMOHsCM = list()
#areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
#areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("MC - Colombo")
index = 1
for (moh in areas) {
  results = data.frame()
  test = data.frame()
  cat("***************** ",moh, "   ******************")
  #resultLocation = paste("/media/tharindu/626047056046E001/FYP1/Mathematical/DenguenatorCMandML/results/",moh,"-", date, sep = '')
  #setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
  #modelForParamA = trainTheModel(depth = 10)
  #testTheModel(area = moh, model = modelForParamA)
  predictSEIR(area = moh)
  #incidencesPlotsForSeperateMOHsCM[[index]] = plotIncidencesGraphCM(area = moh)
  index = index + 1
}