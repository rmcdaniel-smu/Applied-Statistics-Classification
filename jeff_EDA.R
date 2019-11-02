install.packages("skimr")
library(skimr)
library(tidyverse)
library(corrplot)

kobe = project2KobeData

skim(kobe)

table(kobe$shot_type)
table(kobe$season)
table(kobe$shot_zone_area)
table(kobe$shot_zone_range)

###########################################################################################
#Correlation Matrix----
#Select Numeric Values
numericVars = select_if(kobe, is.numeric) %>% select(-14)
names(numericVars)

#Create Correlation matirx
corValues = cor(numericVars)
corrplot(corValues, method = "number")

#Create Sorted List of Correlation Values
corList = as.data.frame(as.table(corValues))
corList[order(corList$Freq),]
