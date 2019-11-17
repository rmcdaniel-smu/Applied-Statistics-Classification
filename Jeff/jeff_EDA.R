library(pacman)
p_load(skimr,tidyverse, corrplot, MASS,plotly)

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
options(scipen = 999)

#Create Sorted List of Correlation Values
corList = as.data.frame(as.table(corValues))
corList[order(corList$Freq),]

###########################################################################################

#action_type
ggplot(kobe, aes(x = action_type))+
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#combined_shot_type
ggplot(kobe, aes(x = combined_shot_type))+
  geom_bar()

#lon and lat
ggplot(kobe, aes(x = lat, y = lon, color = shot_type))+
  geom_point()

#loc_x and loc_y
plot_ly(data = kobe,x = ~loc_x, y = ~loc_y)

threepoint =kobe %>% filter(shot_type == "3PT Field Goal")#(0,0) is a 2 point
plot_ly(data = threepoint,x = ~loc_x, y = ~loc_y)

twopoint =kobe %>% filter(shot_type == "2PT Field Goal") #(31,501),(159,210) need to be converted to 3 point
plot_ly(data=twopoint,x = ~loc_x, y = ~loc_y)

ggplot(kobe, aes(x = loc_x, y = loc_y, color = shot_type))+
  geom_point()

#shot_zone_area
ggplot(kobe, aes(x = shot_zone_area))+
  geom_bar()

#shot_zone_range
ggplot(kobe, aes(x = shot_zone_range))+
  geom_bar()

#Opponent
ggplot(kobe, aes(x = opponent))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#arena_temp
ggplot(kobe, aes(x = arena_temp))+
  geom_histogram()

#avgnoisedb
ggplot(kobe, aes(x = avgnoisedb))+
  geom_histogram()

