#Run Jeff_EDA.R, the data_wrangling.R before running code for models
install.packages('ROCR')
library(pacman)
p_load(skimr,tidyverse, corrplot, MASS,caret)

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
summary(model.stepwise)
model.stepwise$anova
aov(model.stepwise)

#Cross Validation #############################################################################################################
#K-fold CV
Train <- createDataPartition(kobe_clean$shot_made_flag, p=0.75, list=FALSE)
training <- kobe_clean[ Train, ]
testing <- kobe_clean[ -Train, ]

ctrl <- trainControl(method = "repeatedcv",
                     number = 25,
                     repeats = 5,

                     classProbs = T)
  
trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

#paul's train.control code(for dds says paul)-----
#train.Control <- trainControl(method = "repeatedcv",
#                              number = 25,
#                              repeats = 5,
#                              summaryFunction = twoClassSummary,
#                              classProbs = T)

#SPECIFY MULTICOLINARITY SECTION IN THE WRITEUP----

mod_fit <- train(shot_made_flag ~ recId + action_type + minutes_remaining + 
                   period + season + seconds_remaining + shot_distance + shot_zone_area + 
                   shot_zone_basic + shot_id + attendance + arena_temp,  data=kobe_clean, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

#Interpret Coefficients
exp(coef(mod_fit))


#Model Performance ##############################################################################################################
#confusion matrix https://rpubs.com/dvorakt/255527
pred = predict(mod_fit, newdata=training)
confusionMatrix(table(data=as.numeric(pred>0.5), training$shot_made_flag))


#ROC/AUC
library(ROCR)
library(Metrics)
# Compute AUC for predicting Class with the model
pred <- prediction(pred, training$shot_made_flag)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Log Loss Function ##############################################################################################################
#We may need to change from training to orignal dataset?????

training$prob = predict(mod_fit, newdata=training)
loglossTraining = training %>%
  mutate(logloss = training$shot_made_flag * log(1-training$prob) + (1-training$shot_made_flag)*log(1-training$prob))

#Will generate log loss value
loglossValue = -1/15523 * sum(loglossTraining$logloss)
loglossValue