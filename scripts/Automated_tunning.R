require(ggm)
optimumvalues = data.frame(withIh = numeric(0), withMob = numeric(0), withFeatureReduction = numeric(0), bestRounds = numeric(0), bestDepth = numeric(0), bestLearningRate = numeric(0), bestRMSE = numeric(0), bestR2 = numeric(0))
optValsIndex = 1
# header[1:8] = NA
# header[1,] = c("withIh", "withMob", "withFeatureReduction", "bestRounds", "bestDepth", "bestLearningRate", "bestRMSE", "bestR2")

# write.table(header, file = "results.csv", append = TRUE, row.names = F, col.names = F, sep = ",")

roundsRange = seq(1000, 1600, 200)
depthRange = 2:10
learningrateRange = seq(0.005, 0.01, 0.001)

for(withIh in 0:1) {
  for(withFeatureReduction in 0:2) {
    for(withMob in 0:1) {
      
      
      results = data.frame()
      test = data.frame()
      for (moh in areas) {
        cat("***************** ",moh, "   ******************    withIh = ", withIh, ", withMob = ", withMob, ", withFeatureReduction = ", withFeatureReduction, fill = T)
        resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/", moh, sep = '')
        setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh, withmobility = withMob, withcaselags = withIh)
      }
      
      if(withFeatureReduction != 0) {
        featureReduction(withFeatureReduction)
      }
      
      R2 <<- 0
      bestR2 = 0
      bestRMSE = 0
      bestDepth = 0
      bestRounds = 0
      bestLearningRate = 0
      
      
      for(rounds in roundsRange) {
        for(depth in depthRange) {
          for(learningrate in learningrateRange) {
            modelForParamA = trainTheModel(rounds = rounds, depth = depth, learningRate = learningrate, verbose = 0, threads = 4)
            
            for (moh in areas) {
              cat("***************** ",moh, "   ******************", fill = T)
              predictSEIR(area = moh)
              getRMSE(area = moh)
            }
            
            if(R2 > bestR2) {
              bestR2 <<- R2
              bestRMSE <<- RMSE
              bestRounds <<- rounds
              bestDepth <<- depth
              bestLearningRate <<- learningrate
            }      
          }  
        }  
      }  
      # line = data.frame(1)
      # line[1:8] = NA
      optimumvalues[optValsIndex,] = c(withIh, withMob, withFeatureReduction, bestRounds, bestDepth, bestLearningRate, bestRMSE, bestR2)
      optValsIndex <<- optValsIndex+1
      # write.table(line, file = "results.csv", append = T, row.names = F, col.names = F, sep = ",")
    }
  }
}

featureReduction = function(reductionCriteria) {
  ## PCorrelation
  if(reductionCriteria == 1) {
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
  }
  
  ## Correlation
  if(reductionCriteria == 2) {
    correlation = as.data.frame(abs(cor(data.frame(results[,!(names(results) %in% c("moh_name", "population", "year", "best.a"))]), results$best.a)))
    correlation = data.frame(feature = row.names(correlation), correlation = correlation$V1, stringsAsFactors = F)
    
    selectedRows <<- c(correlation[correlation$correlation > round(median(correlation$correlation), 3),]$feature, "best.a", "moh_name", "year", "population")
  }

  results <<- results[, (names(results) %in% selectedRows)]
  test <<- test[, (names(test) %in% selectedRows)]
}

getRMSE = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  cat("RMSE = ", RMSE, fill = T)
  data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  R2 <<- 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  cat("R2 = ", R2, fill = T)
}

