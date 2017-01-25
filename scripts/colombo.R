area = 181
tempArea = 138
currentMOH = data.frame(cases = melt(dengueData2013[area,][,3:54])$value, 
                        temperature = melt(temperatureData2013[tempArea,][,3:54])$value,
                        rainfall = melt(rainfallData2013[152,][,2:53])$value
                        #dehiwala = melt(dengueData2013[55,][,3:54])$value, 
                        #colomboMOH = melt(dengueData2013[181,][,3:54])$value, 
                        #                        kolonnawa = melt(dengueData2013[142,][,3:54])$value, 
                        #                       kaduwela = melt(dengueData2013[111,][,3:54])$value, 
                        #moratuwaMOH = melt(dengueData2013[200,][,3:54])$value
                        #                      kaduwela = melt(dengueData2013[235,][,3:54])$value
)
#...................................................................#

# Add columns
currentMOH$lastWeekCases = c(dengueData2014[area,][3:54]$`52`, currentMOH$cases[1:51])
#currentMOH$casesLag2[3:52] = c(currentMOH$cases[1:50])
#currentMOH$casesLag3[4:52] = c(currentMOH$cases[1:49])
#currentMOH$casesLag4[5:52] = c(currentMOH$cases[1:48])
currentMOH$dehiwalaMOHLag1[2:52] = c(melt(dengueData2013[55,][,3:54])$value[1:51])
currentMOH$dehiwalaMOHLag2[3:52] = c(melt(dengueData2013[55,][,3:54])$value[1:50])
#currentMOH$dehiwalaMOHLag3[4:52] = c(melt(dengueData2013[55,][,3:54])$value[1:49])
#currentMOH$dehiwalaMOHLag4[5:52] = c(melt(dengueData2013[55,][,3:54])$value[1:48])
#currentMOH$moratuwaMOHLag1[2:52] = c(melt(dengueData2013[200,][,3:54])$value[1:51])
#currentMOH$moratuwaMOHLag2[3:52] = c(melt(dengueData2013[200,][,3:54])$value[1:50])
#currentMOH$moratuwaMOHLag3[4:52] = c(melt(dengueData2013[200,][,3:54])$value[1:49])
#currentMOH$moratuwaMOHLag4[5:52] = c(melt(dengueData2013[200,][,3:54])$value[1:48])


#currentMOH$tempLag1[2:52] = c(currentMOH$temperature[1:51])
#currentMOH$tempLag2[3:52] = c(currentMOH$temperature[1:50])
#currentMOH$tempLag3[4:52] = c(currentMOH$temperature[1:49])
#currentMOH$tempLag4[5:52] = c(currentMOH$temperature[1:48])
currentMOH$tempLag5[6:52] = c(currentMOH$temperature[1:47])
currentMOH$tempLag6[7:52] = c(currentMOH$temperature[1:46])
#currentMOH$tempLag7[8:52] = c(currentMOH$temperature[1:45])
#currentMOH$tempLag8[9:52] = c(currentMOH$temperature[1:44])

for(i in 1:ncol(currentMOH)){
  currentMOH[is.na(currentMOH[,i]), i] <- mean(currentMOH[,i], na.rm = TRUE)
}
#...................................................................#

#Visualizing Data
data = data.frame(week = c(1:52), actualCases = as.numeric(unCleanedDengueData2013[area,][3:54]), cleanedCases = as.numeric(cleanedDengueData2013[area,][3:54]))
dmelt = melt(data, id = "week")

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  scale_fill_discrete(name="",
                      breaks=c("actualCases", "cleanedCases"),
                      labels=c("Actual Data", "Cleaned Data")) +
  ggtitle("Dengue Incidences 2013 - Colombo MOH")




area = 181
tempArea = 138
rainArea = 152
moh1 = 55
moh2 = 200
currentMOH = data.frame(cases = melt(dengueData2013_2014[area,][,3:106])$value, 
                        temperature = melt(temperatureData2013[tempArea,][,3:54])$value,
                        rainfall = melt(rainfallData2013[rainArea,][,2:53])$value
)
#...................................................................#

# On 2013_2014 data
# Chose a specific MOH
currentMOH$lastWeekCases = c((currentMOH$cases[1] + currentMOH$cases[53])/2, currentMOH$cases[1:103])
#currentMOH$casesLag2 = c((currentMOH$cases[2:1] + currentMOH$cases[54:53])/2, currentMOH$cases[1:102])
currentMOH$casesLag3 = c((currentMOH$cases[3:1] + currentMOH$cases[55:53])/2, currentMOH$cases[1:101])
#currentMOH$casesLag4 = c((currentMOH$cases[4:1] + currentMOH$cases[56:53])/2, currentMOH$cases[1:100])
currentMOH$dehiwalaMOHLag1 = c(as.numeric(dengueData2013_2014[moh1,][,3] + dengueData2013_2014[moh1,][,55])/2, 
                              as.numeric(dengueData2013_2014[moh1,][,3:105]))
#currentMOH$dehiwalaMOHLag2 = c(as.numeric(dengueData2013_2014[moh1,][,4:3] + dengueData2013_2014[moh1,][,56:55])/2, 
#                              melt(dengueData2013_2014[moh1,][,3:106])$value[1:102])
#currentMOH$dehiwalaMOHLag3 = c(as.numeric(dengueData2013_2014[moh1,][,5:3] + dengueData2013_2014[moh1,][,57:55])/2, 
#                              melt(dengueData2013_2014[moh1,][,3:106])$value[1:101])
#currentMOH$dehiwalaMOHLag4 = c(as.numeric(dengueData2013_2014[moh1,][,6:3] + dengueData2013_2014[moh1,][,58:55])/2, 
#                              melt(dengueData2013_2014[moh1,][,3:106])$value[1:100])
#currentMOH$moratuwaMOHLag1 = c(as.numeric(dengueData2013_2014[moh2,][,3] + dengueData2013_2014[moh2,][,55])/2, 
#                              melt(dengueData2013_2014[moh2,][,3:106])$value[1:103])
currentMOH$moratuwaMOHLag2 = c(as.numeric(dengueData2013_2014[moh2,][,4:3] + dengueData2013_2014[moh2,][,56:55])/2, 
                               melt(dengueData2013_2014[moh2,][,3:106])$value[1:102])
#currentMOH$moratuwaMOHLag3 = c(as.numeric(dengueData2013_2014[moh2,][,5:3] + dengueData2013_2014[moh2,][,57:55])/2, 
#                               melt(dengueData2013_2014[moh2,][,3:106])$value[1:101])
#currentMOH$moratuwaMOHLag4 = c(as.numeric(dengueData2013_2014[moh2,][,6:3] + dengueData2013_2014[moh2,][,58:55])/2, 
#                               melt(dengueData2013_2014[moh2,][,3:106])$value[1:100])


currentMOH$tempLag1 = c(currentMOH$temperature[52], currentMOH$temperature[1:51])
#currentMOH$tempLag2 = c(currentMOH$temperature[51:52], currentMOH$temperature[1:50])
currentMOH$tempLag3 = c(currentMOH$temperature[50:52],currentMOH$temperature[1:49])
currentMOH$tempLag4 = c(currentMOH$temperature[49:52],currentMOH$temperature[1:48])
#currentMOH$tempLag5 = c(currentMOH$temperature[48:52],currentMOH$temperature[1:47])
#currentMOH$tempLag6 = c(currentMOH$temperature[47:52],currentMOH$temperature[1:46])
#currentMOH$tempLag7 = c(currentMOH$temperature[46:52],currentMOH$temperature[1:45])
#currentMOH$tempLag8 = c(currentMOH$temperature[45:52],currentMOH$temperature[1:44])
#...................................................................#

