for(mohName in areas) {
  cat(mohName, "  ", cor(results[results$moh_name==mohName & results$day > 10,]$best.ih, results[results$moh_name==mohName & results$day > 10,]$best.eh), fill = T)
  plot(results[results$moh_name==mohName & results$year==year & results$day > 5,]$best.ih, results[results$moh_name==mohName & results$year==year & results$day > 5,]$best.eh, main = mohName)
}


## Combining MOHs' district wise
dengue2012_districts = data.frame(id=NA, district_name = NA)
dengue2012_districts[,3:55] = NA
colnames(dengue2012_districts)[3:55] = colnames(dengue2012)[3:55]
dengue2012_districts = dengue2012_districts[-1,]
dengue2013_districts = data.frame(id=NA, district_name = NA)
dengue2013_districts[,3:55] = NA
colnames(dengue2013_districts)[3:55] = colnames(dengue2013)[3:55]
dengue2013_districts = dengue2013_districts[-1,]
dengue2014_districts = data.frame(id=NA, district_name = NA)
dengue2014_districts[,3:55] = NA
colnames(dengue2014_districts)[3:55] = colnames(dengue2014)[3:55]
dengue2014_districts = dengue2014_districts[-1,]

districts = c("Colombo", "Kandy", "Galle")
years = 2012:2014

for (year in years) {
  for (districtName in districts) {
    denDistrictFrame = data.frame()
    denMohFrame = data.frame()
    
    if(year == 2012) {
      denDistrictFrame = dengue2012_districts
      denMohFrame = dengue2012
    }
    
    if(year == 2013) {
      denDistrictFrame = dengue2013_districts
      denMohFrame = dengue2013
    }
    
    if(year == 2014) {
      denDistrictFrame = dengue2014_districts
      denMohFrame = dengue2014
    }
    
    temp_mohs_in_district = districtsAndMOHs[districtsAndMOHs$District == districtName,]$MOH_name
    temp_districts_dengue = denMohFrame[denMohFrame$MOH_name %in% temp_mohs_in_district,]
    denDistrictFrame[nrow(denDistrictFrame)+1,] = c(-1, districtName, apply(temp_districts_dengue[3:55], 2, sum)[1:53])
    
    if(year == 2012) {
      dengue2012_districts = denDistrictFrame
      dengue2012 = denMohFrame
    }
    
    if(year == 2013) {
      dengue2013_districts = denDistrictFrame
      dengue2013 = denMohFrame
    }
    
    if(year == 2014) {
      dengue2014_districts = denDistrictFrame
      dengue2014 = denMohFrame
    }
  }
}

for(column in 3:55) {
  class(dengue2012_districts[,column]) = "numeric"
  class(dengue2013_districts[,column]) = "numeric"
  class(dengue2014_districts[,column]) = "numeric"
}

write.csv(x = dengue2012_districts, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengue2012_districts.csv", row.names = FALSE)
write.csv(x = dengue2013_districts, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengue2013_districts.csv", row.names = FALSE)
write.csv(x = dengue2014_districts, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengue2014_districts.csv", row.names = FALSE)


## Combining populations district wise
populations_districts = data.frame("DIST_ID" = NA, "DIST_CODE" = NA, "DIST_NAME" = NA, "RDHS_ID" = NA, "ESTM_POP" = NA, "actual_POP" = NA)
populations_districts = populations_districts[-1,]

for (districtName in districts) {
  temp_mohs_in_district = districtsAndMOHs[districtsAndMOHs$District == districtName,]$MOH_name
  temp_mohs_populations = populations[populations$MOH_NAME %in% temp_mohs_in_district,]
  populations_districts[nrow(populations_districts)+1,] = c(-1, -1, districtName, -1, apply(temp_mohs_populations[5], 2, sum), apply(temp_mohs_populations[6], 2, sum))
}
write.csv(x = populations_districts, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Population/populations_districts.csv", row.names = FALSE)


## Combining mobility district wise
mobilityTripsFactorized_districts = mobilityTripsFactorized[-c(1:nrow(mobilityTripsFactorized)),]
colnames(mobilityTripsFactorized_districts)[1] = "DIST_NAME"

for (districtName in districts) {
  temp_mohs_in_district = districtsAndMOHs[districtsAndMOHs$District == districtName,]$MOH_name
  temp_mohs_mobility = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME %in% temp_mohs_in_district,]
  aggregated_mobility_dataframe = data.frame(DIST_NAME = districtName, aggregate.data.frame(temp_mohs_mobility[3:5], by = list(WEEK_NUMBER = temp_mohs_mobility$WEEK_NUMBER), FUN = sum), stringsAsFactors = F)
  mobilityTripsFactorized_districts = joinFrames(mobilityTripsFactorized_districts, aggregated_mobility_dataframe)
}
write.csv(x = mobilityTripsFactorized_districts, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorized_districts.csv", row.names = FALSE)




## Partial correlations
require(ggm)
# train_names = names(trainingDataFrame)
train_names = names(results)
# train_names = train_names[!(train_names %in% c("moh_name", "year", "population"))]
train_names = train_names[!(train_names %in% c("moh_name", "year", "population", "best.a"))]
train_names = c("best.a", train_names)

positions = 1
secondVariable = 2
thirdVariable = 3
# z = 2:32
z = 2:length(train_names)
bestPcorrelation = 0
bestSecondVar = numeric(0)
bestOtherVars = numeric(0)
bestPcorrelations = data.frame(variable = character(0), changing_variables = character(0), p_correlation = numeric(0), stringsAsFactors = F)
i = 1

for(secondVariable in 2:length(train_names)) {
  tempBestPcorrelation <- 0
  tempBestSecondVar <- secondVariable
  tempBestOtherVars <- secondVariable
  # for (positions in 1:(length(train_names) - 2)) {
  # for (positions in 30) {
  for (positions in 31) {
    currentCols = z[!(z %in% secondVariable)]
    for (thirdVariable in 1:(length(currentCols) - positions + 1)) {
      changingVariables = numeric(0)
      for(i in 1:positions){
        changingVariables = c(changingVariables, train_names[currentCols[thirdVariable+i-1]])
      }
      # tempPcor = abs(pcor(c(train_names[1], train_names[secondVariable], changingVariables), var(trainingDataFrame)))
      tempPcor = abs(pcor(c(train_names[1], train_names[secondVariable], changingVariables), var(results)))
      if (tempPcor > bestPcorrelation) {
        bestPcorrelation = tempPcor
        bestSecondVar = secondVariable
        bestOtherVars = changingVariables
      }
      if (tempPcor > tempBestPcorrelation) {
        tempBestPcorrelation <- tempPcor
        tempBestSecondVar <- secondVariable
        tempBestOtherVars = changingVariables
      }
    }
  }
  i <- i+1
  cat("-----", train_names[secondVariable], "------\n")
  cat(paste(paste(tempBestOtherVars, collapse = ", "), tempBestPcorrelation, sep = " :: "), fill = T)
  cat("\n")
  # 
  # bestPcorrelations[i,1] = train_names[secondVariable]
  # bestPcorrelations[i,2] = length(tempBestOtherVars)
  # bestPcorrelations[i,3] = tempBestPcorrelation
  
  # bestPcorrelations[i,] <- c(train_names[secondVariable], paste(tempBestOtherVars, collapse = ", "), tempBestPcorrelation)
  tempBestPcorrelations = data.frame(variable = train_names[secondVariable], changing_variables = paste(tempBestOtherVars, collapse = ", "), p_correlation = tempBestPcorrelation, stringsAsFactors = F)
  bestPcorrelations = joinFrames(bestPcorrelations, tempBestPcorrelations)
  # bestPcor[i] = tempBestPcorrelation
  # bestSec[i] = train_names[secondVariable]
  # bestOther[i] = paste(tempBestOtherVars, collapse = ", ")
}

# trainingDataFrame = trainingDataFrame[, (names(trainingDataFrame) %in% c(bestPcorrelations[bestPcorrelations$p_correlation > 0.1,]$variable, "cases", "moh_name", "year", "population"))]
selectedRows <<- c(bestPcorrelations[bestPcorrelations$p_correlation > 0.04,]$variable, "best.a", "moh_name", "year", "population")
results = results[, (names(results) %in% selectedRows)]
test = test[, (names(test) %in% selectedRows)]




## Deviding the data to training, validate and test sets in CM
validating_set = results[results$day >= 27 & results$year == 2013, ]
training_set = results[!(results$day >= 27 & results$year == 2013),]
testing_set = test

results = training_set
test = validating_set

temp_results = results## 06:06
temp_test = test
results = joinFrames(results, temp_results)
test = joinFrames(test, temp_test)



## PCor with SEIR as changing variales
require(ggm)
train_names = names(results)
changingVariables = c("best.sh", "best.eh", "best.ih", "best.rh", "day")
train_names = train_names[!(train_names %in% c("moh_name", "year", "population", "best.a", changingVariables))]
train_names = c("best.a", train_names)

bestPcorrelation = 0
bestSecondVar = numeric(0)
bestOtherVars = numeric(0)
bestPcorrelations = data.frame(variable = character(0), changing_variables = character(0), p_correlation = numeric(0), stringsAsFactors = F)
i = 1

for(secondVariable in 2:length(train_names)) {
  tempPcor = abs(pcor(c(train_names[1], train_names[secondVariable], changingVariables), var(results)))
  if (tempPcor > bestPcorrelation) {
    bestPcorrelation = tempPcor
    bestSecondVar = secondVariable
    bestOtherVars = changingVariables
  }
  
  i <- i+1
  cat("-----", train_names[secondVariable], " = ", tempPcor, "------\n")
  cat("\n")
  
  tempBestPcorrelations = data.frame(variable = train_names[secondVariable], changing_variables = paste(changingVariables, collapse = ", "), p_correlation = tempPcor, stringsAsFactors = F)
  bestPcorrelations = joinFrames(bestPcorrelations, tempBestPcorrelations)
}

selectedRows <<- c(bestPcorrelations[bestPcorrelations$p_correlation > round(median(bestPcorrelations$p_correlation), digits = 3),]$variable, "best.a", "moh_name", "year", "population", changingVariables)
results = results[, (names(results) %in% selectedRows)]
test = test[, (names(test) %in% selectedRows)]


## Correlation
correlation = as.data.frame(abs(cor(data.frame(results[,!(names(results) %in% c("moh_name", "population", "year", "best.a"))]), results$best.a)))
correlation = data.frame(feature = row.names(correlation), correlation = correlation$V1, stringsAsFactors = F)

selectedRows <<- c(correlation[correlation$correlation > round(median(correlation$correlation), 3),]$feature, "best.a", "moh_name", "year", "population")
results = results[, (names(results) %in% selectedRows)]
test = test[, (names(test) %in% selectedRows)]
