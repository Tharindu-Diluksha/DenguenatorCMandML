require(ggm)
optimumvalues_ml = data.frame(withIh_ml = numeric(0), withMob_ml = numeric(0), withFeatureReduction_ml = numeric(0), bestRounds_ml = numeric(0), bestDepth_ml = numeric(0), bestLearningRate_ml = numeric(0), bestRMSE_ml = numeric(0), bestR2_ml = numeric(0))
optValsIndex_ml = 1

roundsRange = seq(1000, 1600, 100)
depthRange = 2:10
learningrateRange = seq(0.006, 0.01, 0.001)

# roundsRange = seq(1000, 1600, 200)
# depthRange = 2:10
# learningrateRange = 0.01

for(withIh_ml in 1) {
  for(withFeatureReduction_ml in 0:1) { # 1 = correlation
    for(withMob_ml in 0:1) {

      # for(withIh_ml in 0) {
      #   for(withFeatureReduction_ml in 1) { # 1 = correlation
      #     for(withMob_ml in 0) {
      
      trainingDataFrame <<- data.frame()
      testingDataFrame <<- data.frame()
      for (mohName in areas) {
        cat("***************** ",mohName, "   ******************", fill = T)
        setTrainingAndTestML(mohName = mohName, withcaselags = withIh_ml, withMobility = withMob_ml)
      }
      
      if(withFeatureReduction_ml != 0) {
        featureReduction(withFeatureReduction_ml)
      }
      
      R2 <<- 0
      bestR2_ml = 0
      bestRMSE_ml = 0
      bestDepth_ml = 0
      bestRounds_ml = 0
      bestLearningRate_ml = 0
      
      for(rounds in roundsRange) {
        for(depth in depthRange) {
          for(learningrate in learningrateRange) {
            mlModel = trainTheMLmodel(depth = depth, rounds = rounds, learningRate = learningrate, threads = 4, verbose = 0)
            
            for (index in 1:length(areas)) {
              area = areas[index]
              cat("***************** ",area, "   ******************", fill = T)
              predictions = testTheMLmodel(area = area, model = mlModel)
              getRMSE(area = area, predictions = predictions)
            }
            
            
            if(R2 > bestR2_ml) {
              bestR2_ml <<- R2
              bestRMSE_ml <<- RMSE
              bestRounds_ml <<- rounds
              bestDepth_ml <<- depth
              bestLearningRate_ml <<- learningrate
            }      
          }  
        }  
      }  
      # line = data.frame(1)
      # line[1:8] = NA
      optimumvalues_ml[optValsIndex_ml,] = c(withIh_ml, withMob_ml, withFeatureReduction_ml, bestRounds_ml, bestDepth_ml, bestLearningRate_ml, bestRMSE_ml, bestR2_ml)
      optValsIndex_ml <<- optValsIndex_ml+1
      # write.table(line, file = "results.csv", append = T, row.names = F, col.names = F, sep = ",")
    }
  }
}
write.csv(optimumvalues_ml, file = paste("optimumvalues_ml - ", areas, ".csv"), append = T, row.names = F, col.names = F, sep = ",")

featureReduction = function(reductionCriteria) {
  ## PCorrelation
  # if(reductionCriteria == 1) {
  #   train_names = names(trainingDataFrame)
  #   changingVariables = c("best.sh", "best.eh", "best.ih", "best.rh", "day")
  #   train_names = train_names[!(train_names %in% c("moh_name", "year", "population", "best.a", changingVariables))]
  #   train_names = c("best.a", train_names)
  #   
  #   bestPcorrelation = 0
  #   bestSecondVar = numeric(0)
  #   bestOtherVars = numeric(0)
  #   bestPcorrelations = data.frame(variable = character(0), changing_variables = character(0), p_correlation = numeric(0), stringsAsFactors = F)
  #   i = 1
  #   
  #   for(secondVariable in 2:length(train_names)) {
  #     tempPcor = abs(pcor(c(train_names[1], train_names[secondVariable], changingVariables), var(results)))
  #     if (tempPcor > bestPcorrelation) {
  #       bestPcorrelation = tempPcor
  #       bestSecondVar = secondVariable
  #       bestOtherVars = changingVariables
  #     }
  #     
  #     i <- i+1
  #     cat("-----", train_names[secondVariable], " = ", tempPcor, "------\n")
  #     cat("\n")
  #     
  #     tempBestPcorrelations = data.frame(variable = train_names[secondVariable], changing_variables = paste(changingVariables, collapse = ", "), p_correlation = tempPcor, stringsAsFactors = F)
  #     bestPcorrelations = joinFrames(bestPcorrelations, tempBestPcorrelations)
  #   }
  #   
  #   selectedRows <<- c(bestPcorrelations[bestPcorrelations$p_correlation > round(median(bestPcorrelations$p_correlation), digits = 3),]$variable, "best.a", "moh_name", "year", "population", changingVariables)
  # }
  # 
  ## Correlation
  if(reductionCriteria == 1) {
    correlation = as.data.frame(abs(cor(data.frame(trainingDataFrame[,!(names(trainingDataFrame) %in% c("moh_name", "population", "year", "cases", "week"))]), trainingDataFrame$cases)))
    correlation = data.frame(feature = row.names(correlation), correlation = correlation$V1, stringsAsFactors = F)
    
    selectedRows <<- c(correlation[correlation$correlation > round(median(correlation$correlation), 3),]$feature, "cases", "moh_name", "year", "population", "week")
  }
  
  trainingDataFrame <<- trainingDataFrame[, (names(trainingDataFrame) %in% selectedRows)]
  testingDataFrame <<- testingDataFrame[, (names(testingDataFrame) %in% selectedRows)]
}

getRMSE = function(area, predictions) {
  tempTestML = testingDataFrame[testingDataFrame$moh_name==area,]
  dataML = data.frame(week = tempTestML$week, predicted = predictions, actual = tempTestML$cases)
  R2 <<- 1 - (sum((dataML$actual-dataML$predicted )^2)/sum((dataML$actual-mean(dataML$actual))^2))
  if(R)
  RMSE <<- rmse(predicted = dataML$predicted, actual = dataML$actual)
}



roundsRange = seq(1000, 1600, 100)
depthRange = 2:10
learningrateRange = seq(0.006, 0.01, 0.001)
areas_2 = c("Panadura", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
for (variable in areas_2) {
  areas = variable
  
  optimumvalues_ml = data.frame(withIh_ml = numeric(0), withMob_ml = numeric(0), withFeatureReduction_ml = numeric(0), bestRounds_ml = numeric(0), bestDepth_ml = numeric(0), bestLearningRate_ml = numeric(0), bestRMSE_ml = numeric(0), bestR2_ml = numeric(0))
  optValsIndex_ml = 1
  
  for(withIh_ml in 0:1) {
    for(withFeatureReduction_ml in 0:1) { # 1 = correlation
      for(withMob_ml in 0:1) {
        
        # for(withIh_ml in 0) {
        #   for(withFeatureReduction_ml in 1) { # 1 = correlation
        #     for(withMob_ml in 0) {
        
        trainingDataFrame <<- data.frame()
        testingDataFrame <<- data.frame()
        for (mohName in areas) {
          cat("***************** ",mohName, "   ******************", fill = T)
          setTrainingAndTestML(mohName = mohName, withcaselags = withIh_ml, withMobility = withMob_ml)
        }
        
        if(withFeatureReduction_ml != 0) {
          featureReduction(withFeatureReduction_ml)
        }
        
        R2 <<- -15
        bestR2_ml = 0
        bestRMSE_ml = 0
        bestDepth_ml = 0
        bestRounds_ml = 0
        bestLearningRate_ml = 0
        
        for(rounds in roundsRange) {
          for(depth in depthRange) {
            for(learningrate in learningrateRange) {
              mlModel = trainTheMLmodel(depth = depth, rounds = rounds, learningRate = learningrate, threads = 4, verbose = 0)
              
              for (index in 1:length(areas)) {
                area = areas[index]
                cat("***************** ",area, "   ******************", fill = T)
                predictions = testTheMLmodel(area = area, model = mlModel)
                getRMSE(area = area, predictions = predictions)
              }
              
              
              if(R2 > bestR2_ml) {
                bestR2_ml <<- R2
                bestRMSE_ml <<- RMSE
                bestRounds_ml <<- rounds
                bestDepth_ml <<- depth
                bestLearningRate_ml <<- learningrate
              }      
            }  
          }  
        }  
        # line = data.frame(1)
        # line[1:8] = NA
        optimumvalues_ml[optValsIndex_ml,] = c(withIh_ml, withMob_ml, withFeatureReduction_ml, bestRounds_ml, bestDepth_ml, bestLearningRate_ml, bestRMSE_ml, bestR2_ml)
        optValsIndex_ml <<- optValsIndex_ml+1
        # write.table(line, file = "results.csv", append = T, row.names = F, col.names = F, sep = ",")
      }
    }
  }
  write.csv(optimumvalues_ml, file = paste("results/optimumvalues_ml - ", areas, ".csv"), append = T, row.names = F, col.names = F, sep = ",")
}