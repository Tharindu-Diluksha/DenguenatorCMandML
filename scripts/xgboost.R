require(Matrix)
require(Metrics)
require(xgboost)
require(Ckmeans.1d.dp)
require(ggplot2)

drops <- c("cases", "year", "moh_name")
nround = 2000
max.depth = 10
eta = 0.01
featureNamesML ="c"
featureNamesCM = "c"

## Plot importance matrix graph
plotImportanceGraph = function(featureNames, model) {
  importance_matrix <- xgb.importance(feature_names = featureNames, model = model)
  return(xgb.plot.importance(importance_matrix))
}


############################      Compartmental Model    ##################
trainTheModel = function(rounds=nround, depth=max.depth, learningRate = eta, threads = 5) {
  inputs <<- results[1:(nrow(results)),]
  inputs <<- data.frame(inputs[1:6], inputs[9:length(inputs)])
  inputs <<- data.matrix(inputs)
  inputs <<- inputs[,-6]
  #inputs <<- inputs[,-1]   ## Removing the column "day"
  label <<- results$best.a[1:(nrow(results))]
  featureNamesCM <<- colnames(inputs)
  
  cat("\nDepth = ", depth, fill = T)
  cat("Rounds = ", rounds, fill = T)
  cat("Learning rate = ", eta, fill = T)
  
  model <<- xgboost(data = inputs, label = label, nfold = 1, max.depth = depth, eta = learningRate, nthread = threads, nround = rounds, objective = "reg:linear", maximize = FALSE)

  return(model)  
}

testTheModel = function(area, model) {
  test <<- test[order(test$day),]
  testData <<- data.frame(test[test$moh_name==area,][1:6], test[test$moh_name==area,][9:length(test)])
  actual <<- testData$best.a
  days <<- testData$day
  testData <<- data.matrix(testData)
  testData <<- testData[,-6]
  #testData <<- testData[,-1] ## Removing column "day"
  pred <<- predict(model, testData)
  cat(mse(predicted = pred, actual = actual), fill = T)
  cat(rmsle(predicted = pred, actual = actual), fill = T)
  
  # R Squared
  R2 <- 1 - (sum((actual-pred )^2)/sum((actual-mean(actual))^2))
  cat(R2, fill = T)
  
  return(pred)
}


############################      Machine Learning Model    ##################
trainTheMLmodel = function(rounds=nround, depth=max.depth) {
  drops <- c("cases", "year", "moh_name")
  inputs = trainingDataFrame[,!(names(trainingDataFrame) %in% drops)]
  inputs = data.matrix(inputs)
  label = trainingDataFrame$cases
  featureNamesML <<- colnames(inputs)
  
  model <- xgboost(data = inputs, label = label, nfold = 1, max.depth = max.depth, eta = eta, nthread = 5, nround = rounds, objective = "reg:linear", maximize = FALSE)
  
  #importance_matrix <- xgb.importance(feature_names = colnames(inputs), model = model)
  #xgb.plot.importance(importance_matrix)
  
  return(model)
}

testTheMLmodel = function(area, model) {
  testData = testingDataFrame[testingDataFrame$moh_name==area,]
  actual = testData$cases
  weeks = testData$week
  testData = testData[,!(names(testData) %in% drops)]
  testData = data.matrix(testData)
  pred = predict(model, testData)
  cat("MSE = ", mse(predicted = pred, actual = actual), fill = T)
  cat("RMSLE = ", rmsle(predicted = pred, actual = actual), fill = T)
  
  # R Squared
  R2 <- 1 - (sum((actual-pred )^2)/sum((actual-mean(actual))^2))
  cat("R-Squared = ", R2)
  
  return(pred)
}
