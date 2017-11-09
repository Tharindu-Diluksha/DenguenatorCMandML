require(data.table)
require(sfsmisc)
require(reshape2)

datapath = "media/tharindu/626047056046E001/FYP1/Mathematical/DenguenatorCMandML/data/"
datefile = "data/dengueCases2012.csv"
mohName = "PUTTALAM"
#Read "dengueCases2014.csv"
dengue2012 = fread(datefile, data.table = F, header = F, col.names = c("MOH_id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)

#print(melt(dengue2012[dengue2012$MOH_name==mohName,][,3:54])$MOH_name)
#print((dengue2012[dengue2012$MOH_name=="PUTTALAM"]$c"1"\)
print(dengue2012$Total)