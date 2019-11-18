#Run Jeff_EDA.R, the data_wrangling.R before running code for models

library(pacman)
p_load(skimr,tidyverse, corrplot, MASS)

numericVars_clean = kobe_clean %>% keep(is.numeric)

#remove variables ##############################################################################################################
#single item vairables: team_id,team_name
#High multicolinearity/redundant items: matchup, game_event_id, game_id, lat, lon, combined_shot_type, shot_zone_range

kobe_clean = kobe_clean %>%
  select(-team_id,-team_name,-matchup,-game_event_id,-game_id,-lat,-lon, - combined_shot_type, -shot_zone_range)


#Model with removed variables #################################################################################################

model1 = glm(shot_made_flag ~., family=binomial(link='logit'), data = kobe_clean)
summary(model1)


#Model Selection ##############################################################################################################

#Forward selection
model_backwards<- stepAIC(model1, direction = "forward", trace = F)
summary(model_backwards)
aov(model_backwards)

#Backwards selection
model_backwards<- stepAIC(model1, direction = "backward", trace = F)
summary(model_backwards)
aov(model_backwards)

#Stepwise selection
model_stepwise<- stepAIC(model1, direction = "both", trace = F)
summary(model.stepwise)
model.stepwise$anova
aov(model.stepwise)
