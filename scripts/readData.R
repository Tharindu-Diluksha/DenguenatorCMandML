require(data.table)
require(sfsmisc)
require(reshape2)

datapath = "media/tharindu/626047056046E001/FYP1/Mathematical/DenguenatorCMandML/data/"

#Read "dengueCases2014.csv"
dengue2014 = fread("data/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)

#Read "dengueCases2013.csv"
dengue2013 = fread("data/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)

#Read "dengueCases2012.csv"
dengue2012 = fread("data/dengueCases2012.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)
dengue2012 = dengue2012[-c(239,222, 281),]

# #Read "temp.csv"
# temperatureData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/temp/temp.csv", data.table = F, header = T, stringsAsFactors = F)

# station_temperatureData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/temp/station_temperatures.csv", data.table = F, header = T, stringsAsFactors = F)

# #Read Population
# populations = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Population/Estimated and Actual Populations in MOH's Srilanka2.csv", data.table = F, header = T, stringsAsFactors = F)
# populations = data.frame(sapply(populations[1:2], as.numeric), populations[3], sapply(populations[4:6], as.numeric))
  
# #Read rainFall 
# rainfallData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/rainfall/rainfall2013.csv", data.table = F, header = T, col.names = c("MOH_name", c(1:52)))

# ## Read districts with MOHs
# #districtsAndMOHs = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/MOH/districts and mohs.csv", data.table = F, header = T, stringsAsFactors = F)

# # Get all MOH area names which are common in both dengue 2012 and 2013_2014
# ALL_MOH_NAMES = array(intersect(dengue2013$MOH_name, dengue2012$MOH_name))

#Read mobility - trips
# mobilityTrips2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/weekly_mobility_trips_top10.csv", data.table = F, header = T, stringsAsFactors = F)
# class(mobilityTrips2013$WEEK_NUMBER) = 'numeric'
# class(mobilityTrips2013$MOBILITY_VALUE) = "numeric"
# simpleCap = function(x) {
#   if(x=="MC - Colombo" || x=="Kalutara(North)" || x=="Beruwala(North)" || x=="MC - Galle") {
#     return (x)
#   }
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
#         sep="", collapse=" ")
# }
# mobilityTrips2013 = data.frame(MOH_NAME = sapply(mobilityTrips2013$MOH_NAME, simpleCap), HOME = sapply(mobilityTrips2013$HOME, simpleCap), mobilityTrips2013[3:4])
# mobilityTrips2013 = mobilityTrips2013[(mobilityTrips2013$HOME %in% ALL_MOH_NAMES),]

## Get top 10 moh areas according to mobility
#moh_names_array = array(unique(mobilityTrips2013$HOME[mobilityTrips2013$MOH_NAME=='MC - Colombo'], nmax = 10))
#moh_names_array = intersect(moh_names_array, ALL_MOH_NAMES)

# moh_names_array = array(intersect(dengue2013$MOH_name, dengue2012$MOH_name))

# ## Read Factorized mobility
# # mobilityTripsFactorizedWithReportingRate = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorizedWithReportingRate0.04.csv", data.table = F, header = T, stringsAsFactors = F)
# # mobilityTripsFactorized = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorized.csv", data.table = F, header = T, stringsAsFactors = F)

# ## Read weekly vegetation indices file
# vegetationIndicesWeekly = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/vegetationIndicesWeekly.csv", data.table = F, header = T, stringsAsFactors = F)


# mohs_population = populations$MOH_NAME
# mohs_temperature = temperatureData2013$MOH_name

# mohs_dengue12 = dengue2012$MOH_name
# mohs_dengue13 = dengue2013$MOH_name

# mohs_vegetation = unique(vegetationIndicesWeekly$MOH_name)
#mohs_mobility = unique(mobilityTrips2013$MOH_NAME)
#mohs_districts = districtsAndMOHs$MOH_name
