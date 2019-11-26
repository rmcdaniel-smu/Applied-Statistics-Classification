#Run Jeff_EDA.R, the data_wrangling.R before running code for models
install.packages('ROCR')
library(pacman)
p_load(skimr,tidyverse, corrplot, MASS,caret)

numericVars_clean = kobe_clean %>% keep(is.numeric)

#remove variables ##############################################################################################################
#single item vairables: team_id,team_name
#High multicolinearity/redundant items: matchup, game_event_id, game_id, lat, lon, combined_shot_type, shot_zone_range

kobe_clean = kobe_clean %>%
  dplyr::select(-team_id,-team_name, -combined_shot_type, -shot_zone_range,-matchup,-lat,-lon,-period,-game_id)
 
#jeffs select(-team_id,-team_name,-matchup,-game_event_id,-game_id,-lat,-lon, - combined_shot_type, -shot_zone_range)
#paul's  select(-team_id,-team_name,-action_type, -matchup,-shot_zone_area,-shot_zone_basic, -shot_zone_range,-lat,-lon)

#Model with removed variables #################################################################################################

model1 = glm(shot_made_flag ~., family=binomial(link='logit'), data = kobe_clean)
summary(model1)


#Model Selection ##############################################################################################################

#Forward selection (AIC = 25117)
model_backwards<- stepAIC(model1, direction = "forward", trace = F)
summary(model_backwards)
aov(model_backwards)

#Backwards selection (AIC = 25089)
model_backwards<- stepAIC(model1, direction = "backward", trace = F)
summary(model_backwards)
aov(model_backwards)

#Stepwise selection (AIC = 25089)
model_stepwise<- stepAIC(model1, direction = "both", trace = F)
summary(model_stepwise)
model.stepwise$anova
aov(model.stepwise)

#Cross Validation #############################################################################################################
#K-fold CV
set.seed(100)
Train <- createDataPartition(kobe_clean$shot_made_flag, p=0.75, list=FALSE)
training <- kobe_clean[ Train, ]
testing <- kobe_clean[ -Train, ]

#train for specficity??? option
ctrl <- trainControl(method = "repeatedcv",
                     number = 25,
                     repeats = 5,
                     classProbs = T,
                     metric = "Spec")

#combined shot type used instead of action type - test set has action types that are not in the training set

mod_fit <- train(shot_made_flag ~ recId + action_type + game_event_id + 
                   minutes_remaining + season + seconds_remaining + shot_distance + 
                   shot_zone_area + shot_zone_basic + game_date + shot_id + 
                   attendance + arena_temp + playoffs,  data=training, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

#Interpret Coefficients
mod_fit


#Model Performance ##############################################################################################################

#'Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# recode rare variables to something else
# factor action_type has new levels Running Tip Shot, Tip Layup Shot
testing %>% filter(action_type =='Running Tip Shot')
testing %>% filter(action_type =='Tip Layup Shot')
table(testing$action_type)

testing = testing %>% 
  mutate(action_type = if_else(action_type == "Running Tip Shot", 'Layup Shot', action_type))%>%
  mutate(action_type = if_else(action_type == "Tip Layup Shot", 'Layup Shot', action_type))

#confusion matrix https://rpubs.com/dvorakt/255527
pred = predict(mod_fit, newdata=testing)
cf = confusionMatrix(table(data=as.numeric(pred>0.5), testing$shot_made_flag))
misclassificationRate = (cf$table[2,1]+cf$table[1,2]) / sum(cf$table)
misclassificationRate

#ROC/AUC
library(ROCR)
library(Metrics)
# Compute AUC for predicting Class with the model
pred <- prediction(pred, testing$shot_made_flag)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC Curve")


auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


#Recommended Model
modelFinal = glm(shot_made_flag ~ recId + action_type + game_event_id + 
               minutes_remaining + season + seconds_remaining + shot_distance + 
               shot_zone_area + shot_zone_basic + game_date + shot_id + 
               attendance + arena_temp + playoffs, family=binomial(link='logit'), data = kobe_clean)
summary(modelFinal)
#Log Loss Function ##############################################################################################################
#We may need to change from training to orignal dataset?????

training$prob = predict(mod_fit, newdata=training)
loglossTraining = training %>%
  mutate(logloss = training$shot_made_flag * log(1-training$prob) + (1-training$shot_made_flag)*log(1-training$prob))

#Will generate log loss value
loglossValue = -1/15523 * sum(loglossTraining$logloss)
loglossValue

#generate 
#generate predicitons for whole dataset not just training
write_csv(training,"predictions.csv")
#' record id
#' predicted prob