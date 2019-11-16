library(pacman)
p_load(ggplot2, dplyr, tidyverse, dummies, MASS)

df <- read.csv("C:/Users/Pablo/Desktop/KG6372/paul/modelingKobeData.csv", 
               header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

df[which(df$loc_y > 300),17] <- "3PT Field Goal"
df$shot_type <- ifelse(df$shot_type=="2PT Field Goal", 2, 3)
df <- df %>% subset(select=-c(team_id, team_name, action_type, combined_shot_type, season, shot_zone_area, shot_zone_basic, shot_zone_range, matchup))
df <- df %>% subset(select=-c(team_id, team_name))
df <- df %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.character, as.factor) %>% data.frame()

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