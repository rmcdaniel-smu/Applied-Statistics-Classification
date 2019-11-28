library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

kobe = read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

#Model data
##############################################################################################################

#Recode action_type and combined_shot_type
# kobe[which(kobe$action_type == "Alley Oop Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Cutting Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Driving Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Driving Finger Roll Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Driving Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Driving Reverse Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Driving Slam Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Finger Roll Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Follow Up Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Putback Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Putback Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Putback Slam Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Reverse Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Reverse Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Reverse Slam Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Finger Roll Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Reverse Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Slam Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Running Tip Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Slam Dunk Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Tip Layup Shot"),"action_type"] = "short"
# kobe[which(kobe$action_type == "Tip Shot"),"action_type"] = "short"
# 
# kobe$action_type <- ifelse(kobe$action_type=="short", "short", "long")
# 
# # shots with "Dunk", "Layup", or "Tip" are short:
# kobe[which(kobe$combined_shot_type == "Dunk"),"combined_shot_type"] = "short"
# kobe[which(kobe$combined_shot_type == "Layup"),"combined_shot_type"] = "short"
# kobe[which(kobe$combined_shot_type == "Tip Shot"),"combined_shot_type"] = "short"
# 
# kobe$combined_shot_type <- ifelse(kobe$combined_shot_type=="short", "short", "far")

#factorize variables
kobe_clean = kobe %>%
  mutate(action_type = as.factor(action_type)) %>%
  mutate(combined_shot_type = as.factor(combined_shot_type))%>%
  mutate(season = as.factor(season)) %>%
  mutate(shot_type = as.factor(shot_type)) %>%
  mutate(shot_zone_area=as.factor(shot_zone_area))%>%
  mutate(shot_zone_basic = as.factor(shot_zone_basic))%>%
  mutate(shot_zone_range = as.factor(shot_zone_range))%>%
  mutate(opponent = as.factor(opponent)) 
#%>%
 # mutate(shot_made_flag=as.factor(shot_made_flag))


##############################################################################################################
#recode miscoded shot types
kobe_clean = kobe %>% 
  mutate(shot_type = replace(shot_type, loc_x == 0 & loc_y == 0 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 31 & loc_y == 501 & shot_type == '2PT Field Goal','3PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 159 & loc_y == 210 & shot_type == '2PT Field Goal','3PT Field Goal'))


##############################################################################################################
#verify that recode worked
twopoint =kobe_clean %>% filter(shot_type == "2PT Field Goal")
plot_ly(data=twopoint,x = ~loc_x, y = ~loc_y)

threepoint =kobe_clean %>% filter(shot_type == "3PT Field Goal")#(0,0) is a 2 point
plot_ly(data = threepoint,x = ~loc_x, y = ~loc_y)


