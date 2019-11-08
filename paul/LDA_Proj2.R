library(pacman)
p_load(rrcov, MASS, dplyr)

dfKobe <- read.csv("./modelingKobeData.csv")
dfKobe <- dfKobe[order(dfKobe$shot_made_flag),]

#shot_made_flag = recId, game_event_id, game_id, lat, loc_x, loc_y, lon, minutes_remaining, period, playoffs, season, seconds_remaining, shot_distance, team_id, game_date, shot_id, attendance, arena_temp, avgnoisedb
dfKobe <- dfKobe %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.factor, as.character)
str(dfKobe)

dfKobe.numeric <- dfKobe  %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.factor, as.character) %>% 
  subset(select=-c(action_type, combined_shot_type, season, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range, team_name, matchup, opponent, team_id))
str(dfKobe.numeric)


corrplot::corrplot(cor(dfKobe.numeric)
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

# leaving both playoffs and game_id in the model. They have collinearity, but are different.
# dropping lat and lon for loc_y and loc_x, respectively. loc_# are more descriptive (larger range, sd)
# dropping shot_distance for loc_y
dfKobe.numeric.model <- dfKobe  %>%
  subset(select=-c(action_type, combined_shot_type, season, shot_type, shot_zone_area, shot_zone_basic
                   , shot_zone_range, team_name, matchup, opponent, team_id, lat, lon))

Bartlett_ChiSq <- Wilks.test(shot_made_flag ~ ., data=dfKobe.numeric.model, method = "c", approximation = "Bartlett")


# Wilk's Lambda produces significant p-value in Bartlett's test so we need to use a Quadratic Discriminant Analysis instead of Linear
format(round(Bartlett_ChiSq$p.value, 2), nsmall=4)

MASS::qda(shot_made_flag ~ ., CV=T, data=dfKobe.numeric.model)

kobe.qda <- qda(shot_made_flag ~ ., CV=T, data=dfKobe.numeric.model)

data.frame(mean(kobe.qda$posterior[,1]), mean(kobe.qda$posterior[,2]))

shot_made_flagg <- rbind("0", "1")
proportion <- rbind(mean(kobe.qda$posterior[,1]), mean(kobe.qda$posterior[,2]))
data.frame(shot_made_flagg, proportion) # Class Level Information

#caret::confusionMatrix(table(kobe.qda$posterior[,1], dfKobe$shot_made_flag))

#qda.pred <- predict(kobe.qda)$class