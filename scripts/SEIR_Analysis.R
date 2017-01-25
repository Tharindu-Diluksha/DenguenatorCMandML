require(ggplot2) 

source("scripts/readData.R")
source("scripts/general_utils.R")

reportingRate = 0.04
gammah = 0.5*7
sigmah = 0.12*7

pred = 0
actual = 0

results = data.frame()
test = data.frame()

#########################    Functions - for Compartmental model     ######################
calculateDengueDynamicsWeekly = function(day, sh0, eh0, ih0, rh0, index) {
  testData = data.frame(day = day, 
                        best.sh = sh0, 
                        best.eh = eh0, 
                        best.ih = ih0, 
                        best.rh = rh0, 
                        best.a = 0,
                        #test[index,][7],
                        tempTest[index,][9:ncol(tempTest)], stringsAsFactors = F)
  matrix_test <- data.matrix(testData)
  matrix_test <- matrix_test[,-6]
  pred[index] <<- predict(model, matrix(matrix_test, nrow = 1))
  if(pred[index]<0) {
    pred[index] <<- mean(results$best.a)
  }
  a = pred[index]

  
  sh0 = testData$best.sh
  eh0 = testData$best.eh
  ih0 = testData$best.ih
  rh0 = testData$best.rh

  dSh = -a*sh0
  dEh = a*sh0 - gammah*eh0
  dIh = gammah*eh0 - sigmah*ih0
  dRh = sigmah*ih0
  
  ShEhIhRh = as.integer(array(data = c((sh0+dSh), (eh0+dEh), (ih0+dIh), (rh0+dRh))))
  ShEhIhRh[ShEhIhRh<0] = mean(SEIR$eh)
  
  return(ShEhIhRh)
}

calculateDengueDynamicsWeekly10 = function(day, sh0, eh0, ih0, rh0, index, range) {
  if(index!=0 && index%%range==0) {
    sh0 = tempTest$best.sh[index]
    eh0 = tempTest$best.eh[index]
    ih0 = tempTest$best.ih[index]
    rh0 = tempTest$best.rh[index]
  }
  
  testData = data.frame(day = day, 
                        best.sh = sh0, 
                        best.eh = eh0, 
                        best.ih = ih0, 
                        best.rh = rh0, 
                        best.a = 0,
                        #test[index,][7],
                        tempTest[index,][9:ncol(tempTest)])
  sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
  pred[index] <<- predict(model, sparce_matrix_test)
  if(pred[index]<0) {
    pred[index] <<- mean(results$best.a)
  }
  a = pred[index]
  
  sh0 = testData$best.sh
  eh0 = testData$best.eh
  ih0 = testData$best.ih
  rh0 = testData$best.rh
  
  dSh = -a*sh0
  dEh = a*sh0 - gammah*eh0
  dIh = gammah*eh0 - sigmah*ih0
  dRh = sigmah*ih0
  
  ShEhIhRh = as.integer(array(data = c((sh0+dSh), (eh0+dEh), (ih0+dIh), (rh0+dRh))))
  ShEhIhRh[ShEhIhRh<0] = mean(SEIR$eh)
  
  return(ShEhIhRh)
}

calculateDengueDynamicsRecursively = function(seirDataFrame, gap = 4) {
  if(nrow(seirDataFrame) != (gap+1)) {
    for(week in 0:gap){
      seirDataFrame[week+2,] = calculateDengueDynamicsWeekly(day = week, sh0 = seirDataFrame[week+1,]$sh, eh0 = seirDataFrame[week+1,]$eh, ih0 = seirDataFrame[week+1,]$ih, rh0 = seirDataFrame[week+1,]$rh, index = week+1)
    }
  }
  
  for(week in (gap+1):51) {
    lastAnswer = calculateDengueDynamicsWeekly(day = week-gap, sh0 = seirDataFrame[week-gap+1,]$sh, eh0 = seirDataFrame[week-gap+1,]$eh, ih0 = seirDataFrame[week-gap+1,]$ih, rh0 = seirDataFrame[week-gap+1,]$rh, index = week-gap+1)
    for(offset in 1:gap) {
      lastAnswer = calculateDengueDynamicsWeekly(day = week+offset-gap, sh0 = lastAnswer[1], eh0 = lastAnswer[2], ih0 = lastAnswer[3], rh0 = lastAnswer[4], index = week+offset-gap+1)
    }
    seirDataFrame[week+1,] = lastAnswer
  }
  
  return(seirDataFrame)
}

setTrainingAndTest = function(resultLocation, testLocation, mohName) {
  results1 <<- fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  results2 <<- fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  test1 <<- fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  
  results1 <<- data.frame(sapply(results1[1:7], as.numeric), results1[8], sapply(results1[9], as.numeric), stringsAsFactors = F)
  results2 <<- data.frame(sapply(results2[1:7], as.numeric), results2[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  #results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  test1 <<- data.frame(sapply(test1[1:7], as.numeric), test1[8], sapply(test1[9], as.numeric), stringsAsFactors = F)
  
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  vegetationIndexes = vegetationIndicesWeekly[vegetationIndicesWeekly$MOH_name==mohName,]
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156, veg_index = 1:156)
  cases2012 = melt(dengue2012[dengue2012$MOH_name==mohName,][,3:54])$value
  cases2013 = melt(dengue2013[dengue2013$MOH_name==mohName,][,3:54])$value
  cases2014 = melt(dengue2014[dengue2014$MOH_name==mohName,][,3:54])$value
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  vegIndexes2013 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2013,][,3:54])
  vegIndexes2014 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2014,][,3:54])
  vegIndexes2015 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2015,][,3:54])
  vegIndexes2012 = (vegIndexes2013+vegIndexes2014)/2
  vegIndexes2011 = (vegIndexes2012+vegIndexes2013)/2
  
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2012, vegIndexes2013, vegIndexes2011)
  results1 <<- replaceInitValues(results1)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  results2 <<- replaceInitValues(results2)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  #results3 = replaceInitValues(results3)
  test1 <<- replaceInitValues(test1)
  
  cat("Reporting rate: ", reportingRate, fill = T)
  
  results <<- joinFrames(results, results1)
  results <<- joinFrames(results, results2)
  #results <<- joinFrames(results, results3)
  
  test <<- joinFrames(test, test1)
}

replaceInitValues = function(results1) {
  # Previous cases
  results1$casesLag4 = currentMOH$cases[circularIndex(results1$day, 4, 156)]
  results1$casesLag5 = currentMOH$cases[circularIndex(results1$day, 5, 156)]
  results1$casesLag6 = currentMOH$cases[circularIndex(results1$day, 6, 156)]
  # results1$casesLag8 = currentMOH$cases[circularIndex(results1$day, 8, 156)]
  # results1$casesLag9 = currentMOH$cases[circularIndex(results1$day, 9, 156)]
  # results1$casesLag10 = currentMOH$cases[circularIndex(results1$day, 10, 156)]
  
  # Previous temperatures
  results1$tempLag4 = temperature[circularIndex(results1$day, 4, 52)]
  results1$tempLag5 = temperature[circularIndex(results1$day, 5, 52)]
  results1$tempLag6 = temperature[circularIndex(results1$day, 6, 52)]
  results1$tempLag7 = temperature[circularIndex(results1$day, 7, 52)]
  results1$tempLag8 = temperature[circularIndex(results1$day, 8, 52)]
  results1$tempLag9 = temperature[circularIndex(results1$day, 9, 52)]
  results1$tempLag10 = temperature[circularIndex(results1$day, 10, 52)]
  results1$tempLag11 = temperature[circularIndex(results1$day, 11, 52)]
  results1$tempLag12 = temperature[circularIndex(results1$day, 12, 52)]
  results1$tempLag13 = temperature[circularIndex(results1$day, 13, 52)]
  results1$tempLag14 = temperature[circularIndex(results1$day, 14, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(results1$moh_name)
  current.moh.year = unique(results1$year)
  
  cases2012 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  mobilityMOH = data.frame(cases = 1:156)
  if(current.moh.year==2012) {
    mobilityMOH$cases = c(cases2012, cases2013, cases2011)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2013) {
    mobilityMOH$cases= c(cases2013, cases2014, cases2012)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2014) {
    mobilityMOH$cases = c(cases2014, cases2015, cases2013)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  results1$mobilityLag4 = mobilityMOH$cases[circularIndex(results1$day, 4, 156)]
  results1$mobilityLag5 = mobilityMOH$cases[circularIndex(results1$day, 5, 156)]
  results1$mobilityLag6 = mobilityMOH$cases[circularIndex(results1$day, 6, 156)]
  results1$mobilityLag7 = mobilityMOH$cases[circularIndex(results1$day, 7, 156)]
  results1$mobilityLag8 = mobilityMOH$cases[circularIndex(results1$day, 8, 156)]
  results1$mobilityLag9 = mobilityMOH$cases[circularIndex(results1$day, 9, 156)]
  results1$mobilityLag10 = mobilityMOH$cases[circularIndex(results1$day, 10, 156)]
  results1$mobilityLag11 = mobilityMOH$cases[circularIndex(results1$day, 11, 156)]
  results1$mobilityLag12 = mobilityMOH$cases[circularIndex(results1$day, 12, 156)]
  results1$mobilityLag13 = mobilityMOH$cases[circularIndex(results1$day, 13, 156)]
  
  
  ##Vegetation index lags
  #results1$vegIndexLag4 = currentMOH$veg_index[circularIndex(results1$day, 4, 156)]
  #results1$vegIndexLag5 = currentMOH$veg_index[circularIndex(results1$day, 5, 156)]
  #results1$vegIndexLag6 = currentMOH$veg_index[circularIndex(results1$day, 6, 156)]
  
  return(results1)
}  

predictSEIR = function(area) {
  tempTest <<- test[test$moh_name==area,]
  tempSEIR <<- array(tempTest[1,][2:5])
  SEIR <<- data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
  #SEIR <<- calculateDengueDynamicsRecursively(seirDataFrame = SEIR)
  for(week in 0:(52-1)) {
    tempSEIR <<- calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
    #tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1), 4)
    SEIR[week+2, ] <<- tempSEIR
  }
  cat(rmsle(predicted = pred, actual = actual), fill = T)
  
  return(pred)
}

plotIncidencesGraphCM = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSLE <<- rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  cat(RMSE, fill = T)
  cat(RMSLE)
  
  data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  R2 = 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  R2
  
  dmelt = melt(data, id = "week")
  title = paste("Dengue Incidences ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(results$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(results$year)), collapse = ", ")
  cols = colnames(results)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
         aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line() +
    geom_point()+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  return(incidencesPlot)
}


#########################    Functions - for ML model      ################################
setTrainingAndTestML = function(mohName) {
  
  population = populations[populations$MOH_NAME==mohName, ]$actual_POP
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  vegetationIndexes = vegetationIndicesWeekly[vegetationIndicesWeekly$MOH_name==mohName,]
  
  #currentMOH <<- data.frame(week = 1:156, cases = 1:156)
  cases2012 = melt(dengue2012[dengue2012$MOH_name==mohName,][,3:54])$value
  cases2013 = melt(dengue2013[dengue2013$MOH_name==mohName,][,3:54])$value
  cases2014 = melt(dengue2014[dengue2014$MOH_name==mohName,][,3:54])$value
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  vegIndexes2013 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2013,][,3:54])
  vegIndexes2014 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2014,][,3:54])
  vegIndexes2015 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2015,][,3:54])
  vegIndexes2012 = (vegIndexes2013+vegIndexes2014)/2
  vegIndexes2011 = (vegIndexes2012+vegIndexes2013)/2
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156, veg_index = 1:156)
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  currentMOH$veg_index <<- c(vegIndexes2012, vegIndexes2013, vegIndexes2011)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame1 <<- data.frame(cases = cases2012, week = 1:52, year = 2012, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame1 <<- setColumnsML(trainingDataFrame1)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame2 <<- data.frame(cases = cases2013, week = 1:52, year = 2013, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame2 <<- setColumnsML(trainingDataFrame2)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  #results3 = setColumnsML(results3)
  testingDataFrame1 <<- data.frame(cases = cases2014, week = 1:52, year = 2014, moh_name = mohName, population = population, stringsAsFactors = F)
  testingDataFrame1 <<- setColumnsML(testingDataFrame1)
  
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame1)
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame2)
  #trainingDataFrame <<- joinFrames(trainingDataFrame, results3)
  
  testingDataFrame <<- joinFrames(testingDataFrame, testingDataFrame1)
}

setColumnsML = function(train_test_dataframe) {
  # Previous cases
  train_test_dataframe$casesLag4 = currentMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$casesLag5 = currentMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$casesLag6 = currentMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
  
  # Previous temperatures
  train_test_dataframe$tempLag4 = temperature[circularIndex(train_test_dataframe$week, 4, 52)]
  train_test_dataframe$tempLag5 = temperature[circularIndex(train_test_dataframe$week, 5, 52)]
  train_test_dataframe$tempLag6 = temperature[circularIndex(train_test_dataframe$week, 6, 52)]
  train_test_dataframe$tempLag7 = temperature[circularIndex(train_test_dataframe$week, 7, 52)]
  train_test_dataframe$tempLag8 = temperature[circularIndex(train_test_dataframe$week, 8, 52)]
  train_test_dataframe$tempLag9 = temperature[circularIndex(train_test_dataframe$week, 9, 52)]
  train_test_dataframe$tempLag10 = temperature[circularIndex(train_test_dataframe$week, 10, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(train_test_dataframe$moh_name)
  current.moh.year = unique(train_test_dataframe$year)
  
  cases2012 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  mobilityMOH = data.frame(cases = 1:156)
  if(current.moh.year==2012) {
    mobilityMOH$cases = c(cases2012, cases2013, cases2011)
    #mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2013) {
    mobilityMOH$cases= c(cases2013, cases2014, cases2012)
    #mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2014) {
    mobilityMOH$cases = c(cases2014, cases2015, cases2013)
    #mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  train_test_dataframe$mobilityLag4 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$mobilityLag5 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$mobilityLag6 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
  train_test_dataframe$mobilityLag7 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 7, 156)]
  train_test_dataframe$mobilityLag8 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 8, 156)]
  train_test_dataframe$mobilityLag9 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 9, 156)]
  
  
  ##Vegetation index lags
  train_test_dataframe$vegIndexLag4 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$vegIndexLag5 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$vegIndexLag6 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 6, 156)]
  
  return(train_test_dataframe)
}

plotIncidencesGraphML = function(area, predictions) {
  tempTestML = testingDataFrame[testingDataFrame$moh_name==area,]
  dataML = data.frame(week = tempTestML$week, predicted = predictions, actual = tempTestML$cases)
  R2 = 1 - (sum((dataML$actual-dataML$predicted )^2)/sum((dataML$actual-mean(dataML$actual))^2))
  RMSE = rmse(predicted = dataML$predicted, actual = dataML$actual)
  
  dmelt = melt(dataML, id = "week")
  title = paste("Dengue Incidences ", unique(tempTestML$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(trainingDataFrame$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(trainingDataFrame$year)), collapse = ", ")
  cols = colnames(trainingDataFrame)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
                           aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line() +
    geom_point()+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  return(incidencesPlot)
}
