library("sp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("maptools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
require("ggmap")

gadm <- readRDS("/home/suchira/Downloads/LKA_adm2.rds")
mohShape <- readShapePoly("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/ShapeFiles/SL_MOH.shp")

