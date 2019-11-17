library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

kobe = read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))


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
  mutate(opponent = as.factor(opponent)) %>%
  mutate(shot_made_flag=as.factor(shot_made_flag))


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


##############################################################################################################
#remove variables
#single item vairables: team_id,team_name
#High multicolinearity/redundant items: matchup, game_event_id, game_id, lat, lon, combined_shot_type, shot_zone_range

kobe_clean = kobe_clean %>%
  select(-team_id,-team_name,-matchup,-game_event_id,-game_id,-lat,-lon, - combined_shot_type, shot_zone_range)
