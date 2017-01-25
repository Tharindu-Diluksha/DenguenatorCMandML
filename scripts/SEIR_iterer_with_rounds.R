require(reshape2)

#Extract dengue data for a particular area
reportingRate = 0.04
area = 181
startRow = 3
endRow = 54
weeks = endRow - startRow + 1
currentMOH = data.frame(week = c(1:weeks), 
                        cases = melt(dengue.data[area,][,startRow:endRow])$value/reportingRate)

N = 500000 #Population
ShUpperRange = 0.8*N
ShLowerRange = 0.2*N
EhUpperRange = 200
EhLowerRange = 10
IhUpperRange = 500
IhLowerRange = 10

a.upperValue = 0.001
a.lowerValue = 0.0

MAX.ERROR = 50000
error = MAX.ERROR
total.sq.error = MAX.ERROR

#Number of iterations for value initialization and "a"
n.init.iter = c(1:1000)
n.a.iter = c(1:1000)
#Epidemiology parameters
gammah = 0.5 #per day
sigmah = 0.25 #per day

numberOfRounds = 3
for(index in c(1:numberOfRounds)) {
tempA <- 0
tempSh <- 0
tempEh <- 0
tempIh <- 0
tempRh <- 0
best.temp.error <- 0
best.temp.a <- 0
best.temp.sh <- 0
best.temp.eh <- 0
best.temp.ih <- 0
best.temp.rh <- 0
errors <- 0
a <- 0

best.sh <- 0
best.eh <- 0
best.ih <- 0
best.rh <- 0
best.a <- 0
best.total.sq.error <- 0
best.errors <- 0

sh <- 0
eh <- 0
ih <- 0
rh <- 0

initShEhIhRh = function() {
  sh <<- 0
  eh <<- 0
  ih <<- 0
  rh <<- 0
  a <<- 0
  errors <<- 0
  total.sq.error <<- MAX.ERROR
}

initBestTemporaryValues = function() {
  #best.temp.error <<- 0
  error <<- MAX.ERROR
  best.temp.a <<- 0
  best.temp.sh <<- 0
  best.temp.eh <<- 0
  best.temp.ih <<- 0
  best.temp.rh <<- 0
}

calculateDengueDynamics = function(tempa, day) {
  dSh = -tempa*tempSh[day]
  dEh = tempa*tempSh[day] - gammah*tempEh[day]
  dIh = gammah*tempEh[day] - sigmah*tempIh[day]
  dRh = sigmah*tempIh[day]
  
  tempSh[day+1] <<- tempSh[day] + dSh
  tempEh[day+1] <<- tempEh[day] + dEh
  tempIh[day+1] <<- tempIh[day] + dIh
  tempRh[day+1] <<- tempRh[day] + dRh
}

pb <- txtProgressBar(min = 1, max = length(n.init.iter), style = 3)

for (iteration in n.init.iter) {
  initShEhIhRh()
  while((sh + eh + ih + rh) != N) {
    sh = round(runif(1, ShLowerRange, ShUpperRange))
    eh = round(runif(1, EhLowerRange, EhUpperRange))
    ih = round(runif(1, IhLowerRange, IhUpperRange))
    rh = N - (sh + eh + ih)
  }
  #cat(paste("n.init.iter = ", n.init.iter, " sh = ", sh, ", eh = ", eh, ih = ", ih, rh = ", rh, "\n"))
  setTxtProgressBar(pb, iteration)
  #if (iteration%%100==0) print("Doing n.iteration ",iteration," out of ", n.init.iter[length(n.init.iter)], "in\n")  
  
  for(week in currentMOH$week[1:20]) {
    #if(iteration%%100 == 0) print("week ", week, "\n")
    for (a.iteration in n.a.iter) {
      tempA <- 0
      tempSh <- sh[length(sh)]
      tempEh <- eh[length(eh)]
      tempIh <- ih[length(ih)]
      tempRh <- rh[length(rh)]
      
      #    if (a.iteration%%100==0) cat("Doing a.iteration ",a.iteration," out of ",n.a.iter[length(n.a.iter)]," in week ", week,"\n")  
      
      for(day in c(1:7)) {
        tempA[day] <- runif(1, a.lowerValue, a.upperValue)
        calculateDengueDynamics(tempA[day], day)
      }
      sq.error = (currentMOH$cases[week] - gammah*sum(tempEh[1:7]))^2
      #if(sq.error < 100000){
      # cat("sq.error = ", sq.error, "\n")
      #cat("error = ", error, "\n")
      #}
      if(sq.error < error) {
        error <- sq.error
        #cat("sq.error = ", sq.error)
        best.temp.a <- tempA
        best.temp.sh <- tempSh[2:8]
        best.temp.eh <- tempEh[2:8]
        best.temp.ih <- tempIh[2:8]
        best.temp.rh <- tempRh[2:8]
      }
    }#end of n.a.iter
    
    sh <- append(sh, best.temp.sh)
    eh <- append(eh, best.temp.eh)
    ih <- append(ih, best.temp.ih)
    rh <- append(rh, best.temp.rh)
    a <- append(a, best.temp.a)
    errors[week] <- error
    initBestTemporaryValues()
    
  }#end of all the weeks
  #if(sum(errors, na.rm = T)/length(errors) == 0) {
  # cat("temp.total.sq.error = sum(errors)/length(errors) = " , sum(errors)/length(errors), " week = ", week)
  #  cat("\nsq.error = ", sq.error)
  #}  
  if((length(errors[errors == MAX.ERROR])<2) && (total.sq.error > (temp.total.sq.error = sum(errors, na.rm = T)/length(errors)))) {
    total.sq.error <- temp.total.sq.error
    best.total.sq.error <- total.sq.error
    best.sh <- sh
    best.eh <- eh
    best.ih <- ih
    best.rh <- rh
    best.errors <- errors
    best.a <- a
  }
}
cat("Errors = ", best.total.sq.error)


result = data.frame(day = c(0:(length(best.sh)-1)),
                    best.sh = best.sh, 
                    best.eh = best.eh,
                    best.ih = best.ih,
                    best.rh = best.rh,
                    best.a = best.a)
write.csv(x = result, file = paste("data/result", index,".csv"), sep = ",", row.names = FALSE, col.names = TRUE)
}