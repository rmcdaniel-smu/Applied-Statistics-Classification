library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

kobe = read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

skimr::skim(kobe)

table(kobe$shot_type)
table(kobe$season)
table(kobe$shot_zone_area)
table(kobe$shot_zone_range)

###########################################################################################
#Correlation Matrix----
#Select Numeric Values
numericVars = kobe %>% keep(is.numeric)

#Create Correlation matirx
corValues = cor(numericVars)
corrplot(corValues, method = "number", order = "alphabet")

#Create Sorted List of Correlation Values
corList = as.data.frame(as.table(corValues))
corList[order(corList$Freq),]
