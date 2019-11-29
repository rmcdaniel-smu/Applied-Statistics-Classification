library(pacman)
p_load(skimr,tidyverse, corrplot, MASS,readxl,plotly)

kobe = read_excel("./project2KobeData.xlsx",sheet="modelData")
kobe_predict = read_excel("./project2KobeData.xlsx",sheet="predData")

#Model data
##############################################################################################################

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

kobe_predict = kobe_predict %>%
  mutate(action_type = as.factor(action_type)) %>%
  mutate(combined_shot_type = as.factor(combined_shot_type))%>%
  mutate(season = as.factor(season)) %>%
  mutate(shot_type = as.factor(shot_type)) %>%
  mutate(shot_zone_area=as.factor(shot_zone_area))%>%
  mutate(shot_zone_basic = as.factor(shot_zone_basic))%>%
  mutate(shot_zone_range = as.factor(shot_zone_range))%>%
  mutate(opponent = as.factor(opponent)) 

##############################################################################################################
#recode miscoded shot types
kobe_clean = kobe %>% 
  mutate(shot_type = replace(shot_type, loc_x == 0 & loc_y == 0 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 31 & loc_y == 501 & shot_type == '2PT Field Goal','3PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 159 & loc_y == 210 & shot_type == '2PT Field Goal','3PT Field Goal'))

kobe_predict = kobe_predict %>%
  mutate(shot_type = replace(shot_type, loc_x == 87 & loc_y == 40 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == -194 & loc_y == 123 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == -170 & loc_y == 155 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 49 & loc_y == 220 & shot_type == '3PT Field Goal','2PT Field Goal')) %>% 
  mutate(shot_type = replace(shot_type, loc_x == 110 & loc_y == 192 & shot_type == '3PT Field Goal','2PT Field Goal')) 
##############################################################################################################
#verify that recode worked
twopoint =kobe_predict %>% filter(shot_type == "2PT Field Goal")
plot_ly(data=twopoint,x = ~loc_x, y = ~loc_y)

threepoint =kobe_predict %>% filter(shot_type == "3PT Field Goal")#(0,0) is a 2 point
plot_ly(data = threepoint,x = ~loc_x, y = ~loc_y)
