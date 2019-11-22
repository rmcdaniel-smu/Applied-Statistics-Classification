library(pacman)
p_load(ggplot2, dplyr, tidyverse, dummies, MASS)

df <- read.csv("./modelingKobeData.csv", 
               header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

<<<<<<< HEAD
<<<<<<< HEAD

=======
=======
>>>>>>> 498358101e7aea8b713d51270693b6d64122c1e4
df[which(df$action_type == "Alley Oop Dunk Shot"),2] = "short"
df[which(df$action_type == "Cutting Layup Shot"),2] = "short"
df[which(df$action_type == "Driving Dunk Shot"),2] = "short"
df[which(df$action_type == "Driving Finger Roll Layup Shot"),2] = "short"
df[which(df$action_type == "Driving Layup Shot"),2] = "short"
df[which(df$action_type == "Driving Reverse Layup Shot"),2] = "short"
df[which(df$action_type == "Driving Slam Dunk Shot"),2] = "short"
df[which(df$action_type == "Dunk Shot"),2] = "short"
df[which(df$action_type == "Finger Roll Layup Shot"),2] = "short"
df[which(df$action_type == "Follow Up Dunk Shot"),2] = "short"
df[which(df$action_type == "Layup Shot"),2] = "short"
df[which(df$action_type == "Putback Dunk Shot"),2] = "short"
df[which(df$action_type == "Putback Layup Shot"),2] = "short"
df[which(df$action_type == "Putback Slam Dunk Shot"),2] = "short"
df[which(df$action_type == "Reverse Dunk Shot"),2] = "short"
df[which(df$action_type == "Reverse Layup Shot"),2] = "short"
df[which(df$action_type == "Reverse Slam Dunk Shot"),2] = "short"
df[which(df$action_type == "Running Dunk Shot"),2] = "short"
df[which(df$action_type == "Running Finger Roll Layup Shot"),2] = "short"
df[which(df$action_type == "Running Layup Shot"),2] = "short"
df[which(df$action_type == "Running Reverse Layup Shot"),2] = "short"
df[which(df$action_type == "Running Slam Dunk Shot"),2] = "short"
df[which(df$action_type == "Running Tip Shot"),2] = "short"
df[which(df$action_type == "Slam Dunk Shot"),2] = "short"
df[which(df$action_type == "Tip Layup Shot"),2] = "short"
df[which(df$action_type == "Tip Shot"),2] = "short"
df$action_type <- ifelse(df$action_type=="short", "short", "long")

df[which(df$combined_shot_type == "Jump Shot"),2] = "short"
df[which(df$combined_shot_type == "Dunk"),2] = "short"
df[which(df$combined_shot_type == "Layup"),2] = "short"
df[which(df$combined_shot_type == "Tip Shot"),2] = "short"
df[which(df$combined_shot_type == "Hook Shot"),2] = "short"
df[which(df$combined_shot_type == "Bank Shot"),2] = "short"
df$combined_shot_type <- ifelse(df$combined_shot_type=="short", "short", "long")

df[which(df$loc_y > 300),17] <- "3PT Field Goal"
df$shot_type <- ifelse(df$shot_type=="2PT Field Goal", 2, 3)

#remove one-level factors, 
df <- df %>% subset(select=-c(team_id, team_name, season, shot_zone_area, shot_zone_basic, shot_zone_range, matchup))
df <- df %>% subset(select=-c(team_id, team_name))
df <- df %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.character, as.factor) %>% data.frame()
>>>>>>> 498358101e7aea8b713d51270693b6d64122c1e4

########################## fiele x dummy variable creation##########
#x <- data.frame(df$x)
#x <- suppressWarnings(dummy.data.frame(x))
#levels(df$x)
#colnames(x) <- c("x.x1","x.x2","x.x3")
df <- df %>% subset(select=-c(opponent))

df.numeric <- df %>% keep(is.numeric)

corrplot::corrplot(cor(df.numeric)
                   , title = "Correlation of Predictor Variables, Before Variable Elimination"
                   , type = "lower"
                   , tl.pos = "ld"
                   , method = "square"
                   , tl.cex = 0.65
                   , tl.col = 'red'
                   , order = "alphabet"
                   , diag = F
                   , mar=c(0,0,5,0)
                   , bg="ivory1"
                   ,tl.srt=.05
)

model.forward.Start <- glm(shot_made_flag~1, family=binomial(link='logit'), data = df)

model.Allvar <- glm(shot_made_flag ~ recId + action_type + combined_shot_type + game_event_id + 
                      game_id + lat + loc_x + loc_y + lon + minutes_remaining + period + playoffs + 
                      season + seconds_remaining + shot_distance + shot_type + shot_zone_area + 
                      shot_zone_basic + shot_zone_range + game_date + matchup + opponent + shot_id + 
                      attendance + arena_temp + avgnoisedb, family=binomial(link='logit')
                    , data = df)

#### Forward Selection
model.Forward <- stepAIC(model.forward.Start, direction = "forward", trace = F, scope = formula(model.Allvar))

summary(model.Forward)
model.Forward$anova
#################################### Forward Selection Model Suggestion
forward.glm <- glm(shot_made_flag ~ action_type + attendance + shot_zone_range + arena_temp + 
                     game_event_id + shot_zone_area + season + seconds_remaining + 
                     shot_zone_basic + minutes_remaining + period, family=binomial(link='logit')
                   , data=df)

summary(forward.glm)
forward.glm$aic
########################################################################

# Backward Elimination
model.Backward <- stepAIC(model.Allvar, direction = "backward", trace = F, scope = formula(model.forward.Start))
summary(model.Backward)
model.Backward$anova
#################################### Backward Elimination Model Suggestion
back.glm <- glm(shot_made_flag ~ recId + action_type + combined_shot_type + game_event_id + 
                  game_id + lat + loc_x + loc_y + lon + minutes_remaining + 
                  period + playoffs + season + seconds_remaining + shot_distance + 
                  shot_type + shot_zone_area + shot_zone_basic + shot_zone_range + 
                  game_date + shot_id + attendance + avgnoisedb, family=binomial(link='logit')
                , data=df)

summary(back.glm)
back.glm$aic
########################################################################

# Stepwise Regression
model.Stepwise <- stepAIC(model.Allvar, direction = "both", trace = F)
summary(model.Stepwise)
model.Stepwise$anova
#################################### Stepwise Regression Model Suggestion
step.glm <- glm(shot_made_flag ~ recId + action_type + combined_shot_type + game_event_id + 
                  game_id + lat + loc_x + loc_y + lon + minutes_remaining + 
                  period + playoffs + season + seconds_remaining + shot_distance + 
                  shot_type + shot_zone_area + shot_zone_basic + shot_zone_range + 
                  game_date + shot_id + attendance + avgnoisedb, family=binomial(link='logit')
                , data=df)

summary(step.glm)
step.glm$aic
########################################################################