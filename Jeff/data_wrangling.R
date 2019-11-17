library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

kobe = read.csv("./modelingKobeData.csv", header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

#recode miscoded shot types
kobe_clean = kobe %>% 
  mutate(shot_type = replace(shot_type, loc_x == 0 & loc_y == 0 & shot_type == '3PT Field Goal','2PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 31 & loc_y == 501 & shot_type == '2PT Field Goal','3PT Field Goal')) %>%
  mutate(shot_type = replace(shot_type, loc_x == 159 & loc_y == 210 & shot_type == '2PT Field Goal','3PT Field Goal'))

#verify that recode worked
twopoint =kobe_clean %>% filter(shot_type == "2PT Field Goal")
plot_ly(data=twopoint,x = ~loc_x, y = ~loc_y)

threepoint =kobe_clean %>% filter(shot_type == "3PT Field Goal")#(0,0) is a 2 point
plot_ly(data = threepoint,x = ~loc_x, y = ~loc_y)
