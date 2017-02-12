require(ggplot2) 

source("scripts/readData.R")
source("scripts/general_utils.R")

exag_rate = 1

reportingRate = 0.04
gammah = 0.5*7
sigmah = 0.12*7

pred = 0
actual = 0

results = data.frame()
test = data.frame()

#########################    Functions - for Compartmental model     ######################
calculateDengueDynamicsWeekly = function(day, sh0, eh0, ih0, rh0, index) {
  # tempTest2 = tempTest[, (names(test) %in% c(bestPcorrelations[bestPcorrelations$p_correlation > 0.02,]$variable, "best.a", "year", "population"))]
  testData = data.frame(
                        day = day,
                        best.sh = sh0,
                        best.eh = eh0,
                        best.ih = ih0,
                        best.rh = rh0,
                        best.a = 0,
                        #test[index,][7],
                        # tempTest[index,][9:ncol(tempTest)], stringsAsFactors = F
                        tempTest[index,!(names(tempTest) %in% c("day", "best.sh", "best.eh", "best.ih", "best.rh", "best.a", "moh_name"))], stringsAsFactors = F)
  matrix_test <- data.matrix(testData)
  matrix_test <- matrix_test[,-6]
  # matrix_test <- matrix_test[,-c(1:6)]
  pred[index] <<- predict(model, matrix(matrix_test, nrow = 1))/exag_rate
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

setTrainingAndTest = function(resultLocation, testLocation, mohName, withmobility = T, withcaselags = F) {
  results1 <<- fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  results2 <<- fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  test1 <<- fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  # test1 <<- fread(paste(testLocation,"SEIRAnalysis2013TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  
  results1 <<- data.frame(sapply(results1[1:7], as.numeric), results1[8], sapply(results1[9], as.numeric), stringsAsFactors = F)
  results2 <<- data.frame(sapply(results2[1:7], as.numeric), results2[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  #results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  test1 <<- data.frame(sapply(test1[1:7], as.numeric), test1[8], sapply(test1[9], as.numeric), stringsAsFactors = F)
  
  # results1 <<- results1[-(grep("moh_name", results1$moh_name)),]
  # results2 <<- results2[-(grep("moh_name", results2$moh_name)),]
  # test1 <<- test1[-(grep("moh_name", test1$moh_name)),]
  
  # results2 <<- results1[results1$year==2013,]
  # results1 <<- results1[results1$year==2012,]
  # test1 <<- test1[test1$year==2014,]

  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  rainfall <<- melt(rainfallData2013[rainfallData2013$MOH_name==mohName,][,2:53])$value
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
  
  ih2011 = 0
  ih2012 = 0
  ih2013 = 0
  ih2014 = 0
  ih2015 = 0
  for(i in 1:51) {
    tempIh12 = unique(results1[results1$moh_name==mohName & results1$day==i ,]$best.ih)
    tempIh13 = unique(results2[results2$moh_name==mohName & results2$day==i ,]$best.ih)
    tempIh14 = unique(test1[test1$moh_name==mohName & test1$day==i ,]$best.ih)
    ih2012[i] = ifelse(length(tempIh12 > 1), sum(tempIh12)/length(tempIh12), tempIh12) 
    ih2013[i] = ifelse(length(tempIh13 > 1), sum(tempIh13)/length(tempIh13), tempIh13) 
    ih2014[i] = ifelse(length(tempIh14 > 1), sum(tempIh14)/length(tempIh14), tempIh14) 
  }
  tempIh13 = unique(results2[results2$moh_name==mohName & results2$day==0 ,]$best.ih)
  tempIh14 = unique(test1[test1$moh_name==mohName & test1$day==0 ,]$best.ih)
  ih2012[52] = ifelse(length(tempIh13 > 1), sum(tempIh13)/length(tempIh13), tempIh13) 
  ih2013[52] = ifelse(length(tempIh14 > 1), sum(tempIh14)/length(tempIh14), tempIh14) 
  
  ih2011 =  (ih2012+ih2013)/2
  ih2015 =  (ih2013[1:51]+ih2014)/2
  ih2015[52] = NA
  ih2014[52] = NA
  
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2012, vegIndexes2013, vegIndexes2011)
  currentMOH$ih <<- c(ih2012, ih2013, ih2011)
  results1 <<- replaceInitValues(results1, withmobility, withcaselags)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  currentMOH$ih <<- c(ih2013, ih2014, ih2012)
  results2 <<- replaceInitValues(results2, withmobility, withcaselags)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  currentMOH$ih <<- c(ih2014, ih2015, ih2013)
  #results3 = replaceInitValues(results3, withmobility, withcaselags)
  test1 <<- replaceInitValues(test1, withmobility, withcaselags)
  
  cat("Reporting rate: ", reportingRate, fill = T)
  
  results <<- joinFrames(results, results1)
  results <<- joinFrames(results, results2)
  #results <<- joinFrames(results, results3)
  
  test <<- joinFrames(test, test1)
}

setTrainingAndTest_districts = function(resultLocation, testLocation, districtName) {
  results1 <<- fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  results2 <<- fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  test1 <<- fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #test1 <<- fread(paste(testLocation,"SEIRAnalysis2013TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  
  results1 <<- data.frame(sapply(results1[1:7], as.numeric), results1[8], sapply(results1[9], as.numeric), stringsAsFactors = F)
  results2 <<- data.frame(sapply(results2[1:7], as.numeric), results2[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  #results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  test1 <<- data.frame(sapply(test1[1:7], as.numeric), test1[8], sapply(test1[9], as.numeric), stringsAsFactors = F)
  
  # results1 <<- results1[-(grep("moh_name", results1$moh_name)),]
  # results2 <<- results2[-(grep("moh_name", results2$moh_name)),]
  # test1 <<- test1[-(grep("moh_name", test1$moh_name)),]
  
  # results2 <<- results1[results1$year==2013,]
  # results1 <<- results1[results1$year==2012,]
  # test1 <<- test1[test1$year==2014,]
  
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==districtName,][,3:54])$value
  vegetationIndexes = vegetationIndicesWeekly[vegetationIndicesWeekly$MOH_name==districtName,]
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156, veg_index = 1:156)
  cases2012 = melt(dengue2012_districts[dengue2012_districts$district_name==districtName,][,3:54], measure.vars = 1:52)$value
  cases2013 = melt(dengue2013_districts[dengue2013_districts$district_name==districtName,][,3:54], measure.vars = 1:52)$value
  cases2014 = melt(dengue2014_districts[dengue2014_districts$district_name==districtName,][,3:54], measure.vars = 1:52)$value
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
  results1 <<- replaceInitValues_districts(results1)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  results2 <<- replaceInitValues_districts(results2)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  #results3 = replaceInitValues_districts(results3)
  test1 <<- replaceInitValues_districts(test1)
  
  cat("Reporting rate: ", reportingRate, fill = T)
  
  results <<- joinFrames(results, results1)
  results <<- joinFrames(results, results2)
  #results <<- joinFrames(results, results3)
  
  test <<- joinFrames(test, test1)
}

replaceInitValues = function(training_test_dataframe, withmobility = T, withcaselags = F) {
  # Previous cases
  if(withcaselags) {
    # training_test_dataframe$casesLag4 = currentMOH$cases[circularIndex(training_test_dataframe$day, 4, 156)]
    # training_test_dataframe$casesLag5 = currentMOH$cases[circularIndex(training_test_dataframe$day, 5, 156)]
    # training_test_dataframe$casesLag6 = currentMOH$cases[circularIndex(training_test_dataframe$day, 6, 156)]
    # training_test_dataframe$casesLag7 = currentMOH$cases[circularIndex(training_test_dataframe$day, 7, 156)]
  }
  
  # Previous temperatures
  training_test_dataframe$tempLag4 = temperature[circularIndex(training_test_dataframe$day, 4, 52)]
  training_test_dataframe$tempLag5 = temperature[circularIndex(training_test_dataframe$day, 5, 52)]
  training_test_dataframe$tempLag6 = temperature[circularIndex(training_test_dataframe$day, 6, 52)]
  training_test_dataframe$tempLag7 = temperature[circularIndex(training_test_dataframe$day, 7, 52)]
  training_test_dataframe$tempLag8 = temperature[circularIndex(training_test_dataframe$day, 8, 52)]
  training_test_dataframe$tempLag9 = temperature[circularIndex(training_test_dataframe$day, 9, 52)]
  training_test_dataframe$tempLag10 = temperature[circularIndex(training_test_dataframe$day, 10, 52)]
  training_test_dataframe$tempLag11 = temperature[circularIndex(training_test_dataframe$day, 11, 52)]
  training_test_dataframe$tempLag12 = temperature[circularIndex(training_test_dataframe$day, 12, 52)]
  # training_test_dataframe$tempLag13 = temperature[circularIndex(training_test_dataframe$day, 13, 52)]
  # training_test_dataframe$tempLag14 = temperature[circularIndex(training_test_dataframe$day, 14, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(training_test_dataframe$moh_name)
  current.moh.year = unique(training_test_dataframe$year)
  
  cases2012 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
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
  
  if(withmobility) {
    training_test_dataframe$mobilityLag4 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 4, 156)]
    training_test_dataframe$mobilityLag5 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 5, 156)]
    training_test_dataframe$mobilityLag6 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 6, 156)]
    training_test_dataframe$mobilityLag7 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 7, 156)]
    training_test_dataframe$mobilityLag8 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 8, 156)]
    training_test_dataframe$mobilityLag9 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 9, 156)]
    training_test_dataframe$mobilityLag10 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 10, 156)]
    training_test_dataframe$mobilityLag11 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 11, 156)]
    training_test_dataframe$mobilityLag12 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 12, 156)]
    # training_test_dataframe$mobilityLag13 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 13, 156)]
  }
  ## Ih lags
  # training_test_dataframe$ihLag4 = currentMOH$ih[circularIndex(training_test_dataframe$day, 4, 153)]
  ih = currentMOH$ih[!(is.na(currentMOH$ih))]
  numberOfElementsInArray = length(ih)
  
  if(withcaselags) {
    # training_test_dataframe$ihLag3 = ih[circularIndex(training_test_dataframe$day, 3, numberOfElementsInArray)]
    training_test_dataframe$ihLag4 = ih[circularIndex(training_test_dataframe$day, 4, numberOfElementsInArray)]
    training_test_dataframe$ihLag5 = ih[circularIndex(training_test_dataframe$day, 5, numberOfElementsInArray)]
    training_test_dataframe$ihLag6 = ih[circularIndex(training_test_dataframe$day, 6, numberOfElementsInArray)]
    training_test_dataframe$ihLag7 = ih[circularIndex(training_test_dataframe$day, 7, numberOfElementsInArray)]
  }
    
  ##Vegetation index lags
  training_test_dataframe$vegIndexLag4 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 4, 156)]
  training_test_dataframe$vegIndexLag5 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 5, 156)]
  training_test_dataframe$vegIndexLag6 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 6, 156)]
  training_test_dataframe$vegIndexLag7 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 7, 156)]
  training_test_dataframe$vegIndexLag8 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 8, 156)]
  training_test_dataframe$vegIndexLag9 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 9, 156)]
  training_test_dataframe$vegIndexLag10 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 10, 156)]
  training_test_dataframe$vegIndexLag11 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 11, 156)]
  training_test_dataframe$vegIndexLag12 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 12, 156)]
  
  ## Rainfall
  training_test_dataframe$rainfallLag4 = rainfall[circularIndex(training_test_dataframe$day, 4, 52)]
  training_test_dataframe$rainfallLag5 = rainfall[circularIndex(training_test_dataframe$day, 5, 52)]
  training_test_dataframe$rainfallLag6 = rainfall[circularIndex(training_test_dataframe$day, 6, 52)]
  training_test_dataframe$rainfallLag7 = rainfall[circularIndex(training_test_dataframe$day, 7, 52)]
  training_test_dataframe$rainfallLag8 = rainfall[circularIndex(training_test_dataframe$day, 8, 52)]
  training_test_dataframe$rainfallLag9 = rainfall[circularIndex(training_test_dataframe$day, 9, 52)]
  training_test_dataframe$rainfallLag10 = rainfall[circularIndex(training_test_dataframe$day, 10, 52)]
  training_test_dataframe$rainfallLag11 = rainfall[circularIndex(training_test_dataframe$day, 11, 52)]
  training_test_dataframe$rainfallLag12 = rainfall[circularIndex(training_test_dataframe$day, 12, 52)]
  
  return(training_test_dataframe)
}  

replaceInitValues_districts = function(training_test_dataframe) {
  # Previous cases
  training_test_dataframe$casesLag4 = currentMOH$cases[circularIndex(training_test_dataframe$day, 4, 156)]
  training_test_dataframe$casesLag5 = currentMOH$cases[circularIndex(training_test_dataframe$day, 5, 156)]
  training_test_dataframe$casesLag6 = currentMOH$cases[circularIndex(training_test_dataframe$day, 6, 156)]
  # results1$casesLag8 = currentMOH$cases[circularIndex(results1$day, 8, 156)]
  # results1$casesLag9 = currentMOH$cases[circularIndex(results1$day, 9, 156)]
  # results1$casesLag10 = currentMOH$cases[circularIndex(results1$day, 10, 156)]
  
  # Previous temperatures
  # training_test_dataframe$tempLag4 = temperature[circularIndex(training_test_dataframe$day, 4, 52)]
  # training_test_dataframe$tempLag5 = temperature[circularIndex(training_test_dataframe$day, 5, 52)]
  # training_test_dataframe$tempLag6 = temperature[circularIndex(training_test_dataframe$day, 6, 52)]
  # training_test_dataframe$tempLag7 = temperature[circularIndex(training_test_dataframe$day, 7, 52)]
  # training_test_dataframe$tempLag8 = temperature[circularIndex(training_test_dataframe$day, 8, 52)]
  # training_test_dataframe$tempLag9 = temperature[circularIndex(training_test_dataframe$day, 9, 52)]
  # training_test_dataframe$tempLag10 = temperature[circularIndex(training_test_dataframe$day, 10, 52)]
  # training_test_dataframe$tempLag11 = temperature[circularIndex(training_test_dataframe$day, 11, 52)]
  # training_test_dataframe$tempLag12 = temperature[circularIndex(training_test_dataframe$day, 12, 52)]
  # training_test_dataframe$tempLag13 = temperature[circularIndex(training_test_dataframe$day, 13, 52)]
  # training_test_dataframe$tempLag14 = temperature[circularIndex(training_test_dataframe$day, 14, 52)]
  # 
  #Factorized mobility values
  current.moh.name = unique(training_test_dataframe$moh_name)
  current.moh.year = unique(training_test_dataframe$year)
  
  cases2012 = mobilityTripsFactorized_districts[mobilityTripsFactorized_districts$DIST_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorized_districts[mobilityTripsFactorized_districts$DIST_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorized_districts[mobilityTripsFactorized_districts$DIST_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
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
  
  training_test_dataframe$mobilityLag4 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 4, 156)]
  training_test_dataframe$mobilityLag5 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 5, 156)]
  training_test_dataframe$mobilityLag6 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 6, 156)]
  training_test_dataframe$mobilityLag7 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 7, 156)]
  training_test_dataframe$mobilityLag8 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 8, 156)]
  training_test_dataframe$mobilityLag9 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 9, 156)]
  training_test_dataframe$mobilityLag10 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 10, 156)]
  training_test_dataframe$mobilityLag11 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 11, 156)]
  training_test_dataframe$mobilityLag12 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 12, 156)]
  training_test_dataframe$mobilityLag13 = mobilityMOH$cases[circularIndex(training_test_dataframe$day, 13, 156)]
  
  ## Ih lags
  # results1$igLag7 = currentMOH$ih[circularIndex(results1$day, 7, 156)]
  # results1$igLag8 = currentMOH$ih[circularIndex(results1$day, 8, 156)]
  # results1$igLag9 = currentMOH$ih[circularIndex(results1$day, 9, 156)]
  # results1$igLag10 = currentMOH$ih[circularIndex(results1$day, 10, 156)]
  
  ##Vegetation index lags
  # training_test_dataframe$vegIndexLag4 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 4, 156)]
  # training_test_dataframe$vegIndexLag5 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 5, 156)]
  # training_test_dataframe$vegIndexLag6 = currentMOH$veg_index[circularIndex(training_test_dataframe$day, 6, 156)]
  # 
  return(training_test_dataframe)
}  

predictSEIR = function(area) {
  tempTest <<- test[test$moh_name==area,]
  tempSEIR <<- array(tempTest[1,][2:5])
  SEIR <<- data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
  # SEIR <<- calculateDengueDynamicsRecursively(seirDataFrame = SEIR, gap = 10)
  for(week in 0:(52-1)) {
    tempSEIR <<- calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
    #tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1), 4)
    SEIR[week+2, ] <<- tempSEIR
  }
  cat("RMSLE = ", rmsle(predicted = pred, actual = actual), fill = T)
  
  return(pred)
}

plotIncidencesGraphCM = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSLE <<- rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  cat("RMSE = ", RMSE, fill = T)
  cat("RMSLE of Cases = ", RMSLE, fill = T)
  
  data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  R2 <<- 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  cat("R2 = ", R2, fill = T)
  
  dmelt = melt(data, id = "week")
  title = paste("Dengue Incidences ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(results$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(results$year)), collapse = ", ")
  cols = colnames(results)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  ihLags = paste((gsub('ihLag([0-9]+).*', "\\1", cols[grep("ihLag", cols)])), collapse = ", ")
  vegetationLags = paste((gsub('vegIndexLag([0-9]+).*', "\\1", cols[grep("vegIndexLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  rainfallLags = paste((gsub('rainfallLag([0-9]+).*', "\\1", cols[grep("rainfallLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")", "\n ih lags (", ihLags, "), vegetation lags (", vegetationLags, "), rainfall lags (", rainfallLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
         aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title=element_text(size=13), legend.title = element_text(size = 11), legend.text = element_text(size = 11)) +
    geom_line(size = 1) +
    geom_point(size = 2.5)+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 11))+
    guides(colour = guide_legend(override.aes = list(size=2,linetype=0)))
  
  return(incidencesPlot)
}

## Changed titles
plotIncidencesGraphCM2 = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSLE <<- rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  cat("RMSE = ", RMSE, fill = T)
  cat("RMSLE of Cases = ", RMSLE, fill = T)
  
  data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  R2 <<- 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  cat("R2 = ", R2, fill = T)
  
  dmelt = melt(data, id = "week")
  title = paste("Dengue Incidences ", unique(tempTest$year), " - ", area)
  subtitle = paste("R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  
  incidencesPlot <- ggplot(data = dmelt, 
                           aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title=element_text(size=14), legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
    geom_line(size = 1) +
    geom_point(size = 2.5)+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 11))+
    guides(colour = guide_legend(override.aes = list(size=2,linetype=0)))
    # + legend(col = c("r", "b"))
  return(incidencesPlot)
}

## Generate parameter 'a' graph
plotParamA = function(area) {
  ggplot.data = data.frame(week = tempTest$day, predicted = pred, actual = tempTest$best.a)
  dmelt = melt(ggplot.data, id = "week")
  test.rmsle = rmsle(predicted = ggplot.data$predicted, actual = ggplot.data$actual)
  R2 = 1 - (sum((ggplot.data$actual-ggplot.data$predicted )^2)/sum((ggplot.data$actual-mean(ggplot.data$actual))^2))
  
  title = paste("Parameter 'a' ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSLE = ", round(test.rmsle, digits = 5))
  # title = paste("Parameter 'a' for 2014-Prediction by XGBoost - ", area, " RMSLE = ", round(test.rmsle, digits = 5))
  mohs = paste(array(unique(results$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(results$year)), collapse = ", ")
  cols = colnames(results)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  ihLags = paste((gsub('ihLag([0-9]+).*', "\\1", cols[grep("ihLag", cols)])), collapse = ", ")
  vegetationLags = paste((gsub('vegIndexLag([0-9]+).*', "\\1", cols[grep("vegIndexLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")", "\n ih lags (", ihLags, "), vegetation lags (", vegetationLags, ")")
  
  ggplot(data = dmelt, 
         aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line(size = 1) +
    geom_point(size = 2.5)+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
}

plotIncidencesGraphCM_districts = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSLE <<- rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014_districts[dengue2014_districts$district_name==area,][test$day+1+2-test$day[1]]))
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014_districts[dengue2014_districts$district_name==area,][test$day+1+2-test$day[1]]))
  cat("RMSE = ", RMSE, fill = T)
  cat("RMSLE of Cases = ", RMSLE, fill = T)
  
  data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014_districts[dengue2014_districts$district_name==area,][test$day+1+2-test$day[1]]))
  R2 = 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  R2
  cat("R2 = ", R2, fill = T)
  
  dmelt = melt(data, id = "week")
  title = paste("Dengue Incidences ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(results$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(results$year)), collapse = ", ")
  cols = colnames(results)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  ihLags = paste((gsub('ihLag([0-9]+).*', "\\1", cols[grep("ihLag", cols)])), collapse = ", ")
  vegetationLags = paste((gsub('vegIndexLag([0-9]+).*', "\\1", cols[grep("vegIndexLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")", "\n ih lags (", ihLags, "), vegetation lags (", vegetationLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
                           aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line(size = 1) +
    geom_point(size = 2.5)+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    guides(colour = guide_legend(override.aes = list(size=2,linetype=0)))
  
  return(incidencesPlot)
}


#########################    Functions - for ML model      ################################
setTrainingAndTestML = function(mohName, withcaselags = F, withMobility = F) {
  
  population = populations[populations$MOH_NAME==mohName, ]$actual_POP
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  rainfall <<- melt(rainfallData2013[rainfallData2013$MOH_name==mohName,][,2:53])$value
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
  
  ih2011 = 0
  ih2012 = 0
  ih2013 = 0
  ih2014 = 0
  ih2015 = 0
  for(i in 1:51) {
    tempIh12 = unique(results1[results1$moh_name==mohName & results1$day==i ,]$best.ih)
    tempIh13 = unique(results2[results2$moh_name==mohName & results2$day==i ,]$best.ih)
    tempIh14 = unique(test1[test1$moh_name==mohName & test1$day==i ,]$best.ih)
    ih2012[i] = ifelse(length(tempIh12 > 1), sum(tempIh12)/length(tempIh12), tempIh12) 
    ih2013[i] = ifelse(length(tempIh13 > 1), sum(tempIh13)/length(tempIh13), tempIh13) 
    ih2014[i] = ifelse(length(tempIh14 > 1), sum(tempIh14)/length(tempIh14), tempIh14) 
  }
  tempIh13 = unique(results2[results2$moh_name==mohName & results2$day==0 ,]$best.ih)
  tempIh14 = unique(test1[test1$moh_name==mohName & test1$day==0 ,]$best.ih)
  ih2012[52] = ifelse(length(tempIh13 > 1), sum(tempIh13)/length(tempIh13), tempIh13) 
  ih2013[52] = ifelse(length(tempIh14 > 1), sum(tempIh14)/length(tempIh14), tempIh14)
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156, veg_index = 1:156, ih = 1:156)
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  currentMOH$veg_index <<- c(vegIndexes2012, vegIndexes2013, vegIndexes2011)
  currentMOH$ih <<- c(ih2012, ih2013, ih2011)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame1 <<- data.frame(cases = cases2012, week = 1:52, year = 2012, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame1 <<- setColumnsML(trainingDataFrame1, withcaselags = withcaselags, withMobility = withMobility)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  currentMOH$ih <<- c(ih2013, ih2014, ih2012)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame2 <<- data.frame(cases = cases2013, week = 1:52, year = 2013, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame2 <<- setColumnsML(trainingDataFrame2, withcaselags = withcaselags, withMobility = withMobility)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  currentMOH$ih <<- c(ih2014, ih2015, ih2013)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  #results3 = setColumnsML(results3)
  testingDataFrame1 <<- data.frame(cases = cases2014, week = 1:52, year = 2014, moh_name = mohName, population = population, stringsAsFactors = F)
  testingDataFrame1 <<- setColumnsML(testingDataFrame1, withcaselags = withcaselags, withMobility = withMobility)
  
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame1)
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame2)
  #trainingDataFrame <<- joinFrames(trainingDataFrame, results3)
  
  testingDataFrame <<- joinFrames(testingDataFrame, testingDataFrame1)
}

setColumnsML = function(train_test_dataframe, withcaselags = F, withMobility = F) {
  # Previous cases
  # train_test_dataframe$casesLag4 = currentMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
  # train_test_dataframe$casesLag5 = currentMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
  # train_test_dataframe$casesLag6 = currentMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
  
  # Previous temperatures
  # train_test_dataframe$tempLag2 = temperature[circularIndex(train_test_dataframe$week, 2, 52)]
  # train_test_dataframe$tempLag3 = temperature[circularIndex(train_test_dataframe$week, 3, 52)]
  train_test_dataframe$tempLag4 = temperature[circularIndex(train_test_dataframe$week, 4, 52)]
  train_test_dataframe$tempLag5 = temperature[circularIndex(train_test_dataframe$week, 5, 52)]
  train_test_dataframe$tempLag6 = temperature[circularIndex(train_test_dataframe$week, 6, 52)]
  train_test_dataframe$tempLag7 = temperature[circularIndex(train_test_dataframe$week, 7, 52)]
  train_test_dataframe$tempLag8 = temperature[circularIndex(train_test_dataframe$week, 8, 52)]
  train_test_dataframe$tempLag9 = temperature[circularIndex(train_test_dataframe$week, 9, 52)]
  train_test_dataframe$tempLag10 = temperature[circularIndex(train_test_dataframe$week, 10, 52)]
  train_test_dataframe$tempLag11 = temperature[circularIndex(train_test_dataframe$week, 11, 52)]
  # train_test_dataframe$tempLag12 = temperature[circularIndex(train_test_dataframe$week, 10, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(train_test_dataframe$moh_name)
  current.moh.year = unique(train_test_dataframe$year)
  
  cases2012 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorizedWithReportingRate[mobilityTripsFactorizedWithReportingRate$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
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
  
  if(withMobility) {
    # train_test_dataframe$mobilityLag2 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 2, 156)]
    # train_test_dataframe$mobilityLag3 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 3, 156)]
    train_test_dataframe$mobilityLag4 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
    train_test_dataframe$mobilityLag5 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
    train_test_dataframe$mobilityLag6 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
    train_test_dataframe$mobilityLag7 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 7, 156)]
    train_test_dataframe$mobilityLag8 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 8, 156)]
    train_test_dataframe$mobilityLag9 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 9, 156)]
    train_test_dataframe$mobilityLag10 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 10, 156)]
    train_test_dataframe$mobilityLag11 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 11, 156)]
    
  }
  
  ## Ih lags
  ih = currentMOH$ih[!(is.na(currentMOH$ih))]
  numberOfElementsInArray = length(ih)
  
  if(withcaselags) {
    # training_test_dataframe$ihLag3 = ih[circularIndex(training_test_dataframe$day, 3, numberOfElementsInArray)]
    training_test_dataframe$ihLag4 = ih[circularIndex(training_test_dataframe$week, 4, numberOfElementsInArray)]
    training_test_dataframe$ihLag5 = ih[circularIndex(training_test_dataframe$week, 5, numberOfElementsInArray)]
    training_test_dataframe$ihLag6 = ih[circularIndex(training_test_dataframe$week, 6, numberOfElementsInArray)]
    training_test_dataframe$ihLag7 = ih[circularIndex(training_test_dataframe$week, 7, numberOfElementsInArray)]
  }
  
  ##Vegetation index lags
  # train_test_dataframe$vegIndexLag2 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 2, 156)]
  # train_test_dataframe$vegIndexLag3 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 3, 156)]
  train_test_dataframe$vegIndexLag4 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$vegIndexLag5 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$vegIndexLag6 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 6, 156)]
  train_test_dataframe$vegIndexLag7 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 7, 156)]
  train_test_dataframe$vegIndexLag8 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 8, 156)]
  train_test_dataframe$vegIndexLag9 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 9, 156)]
  train_test_dataframe$vegIndexLag10 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 10, 156)]
  train_test_dataframe$vegIndexLag11 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 11, 156)]
  # train_test_dataframe$vegIndexLag12 = currentMOH$veg_index[circularIndex(train_test_dataframe$week, 12, 156)]
  
  ## Rainfall
  training_test_dataframe$rainfallLag4 = rainfall[circularIndex(training_test_dataframe$week, 4, 52)]
  training_test_dataframe$rainfallLag5 = rainfall[circularIndex(training_test_dataframe$week, 5, 52)]
  training_test_dataframe$rainfallLag6 = rainfall[circularIndex(training_test_dataframe$week, 6, 52)]
  training_test_dataframe$rainfallLag7 = rainfall[circularIndex(training_test_dataframe$week, 7, 52)]
  training_test_dataframe$rainfallLag8 = rainfall[circularIndex(training_test_dataframe$week, 8, 52)]
  training_test_dataframe$rainfallLag9 = rainfall[circularIndex(training_test_dataframe$week, 9, 52)]
  training_test_dataframe$rainfallLag10 = rainfall[circularIndex(training_test_dataframe$week, 10, 52)]
  training_test_dataframe$rainfallLag11 = rainfall[circularIndex(training_test_dataframe$week, 11, 52)]
  # training_test_dataframe$rainfallLag12 = rainfall[circularIndex(training_test_dataframe$week, 12, 52)]
  
  
  return(train_test_dataframe)
}

plotIncidencesGraphML = function(area, predictions) {
  tempTestML = testingDataFrame[testingDataFrame$moh_name==area,]
  dataML = data.frame(week = tempTestML$week, predicted = predictions, actual = tempTestML$cases)
  R2 <<- 1 - (sum((dataML$actual-dataML$predicted )^2)/sum((dataML$actual-mean(dataML$actual))^2))
  RMSE <<- rmse(predicted = dataML$predicted, actual = dataML$actual)
  
  dmelt = melt(dataML, id = "week")
  title = paste("Dengue Incidences ", unique(tempTestML$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(trainingDataFrame$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(trainingDataFrame$year)), collapse = ", ")
  cols = colnames(trainingDataFrame)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  ihLags = paste((gsub('ihLag([0-9]+).*', "\\1", cols[grep("ihLag", cols)])), collapse = ", ")
  vegetationLags = paste((gsub('vegIndexLag([0-9]+).*', "\\1", cols[grep("vegIndexLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")", "\n ih lags (", ihLags, "), vegetation lags (", vegetationLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
                           aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line(size = 1) +
    geom_point(size = 2.5)+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    guides(colour = guide_legend(override.aes = list(size=2,linetype=0)))
  
  return(incidencesPlot)
}
