# Naive Bayes & GLM Model

setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

library(dplyr)  
library(caret) 
library(vip)
library(corrplot)
library(XLConnect)
library(lubridate)
library(ranger) 
library(h2o)


scaling_0_100 <- function(x){
  
  min <- min(x)
  
  x <- (x - min)
  
  max <- max(x)
  
  x <- x/max
  
  return(x)
  
}


# Load and Inpsect Data -----------------------------------------------------------------------

data <- read.csv("df_engineered.csv") %>% select(-1) %>% # drop index 
  mutate(date = ymd(date))

wb <- loadWorkbook("Existing_nhl_scores.xlsx")
odds <- readWorksheet(wb, sheet = "odds", header = TRUE) %>%
  mutate(date = as.Date(date))

glimpse(odds)
glimpse(data)


dataB <- data %>% left_join(odds, by = c("date" = "date", "target_team" = "home_team")) %>%
  mutate(favourite = as.factor(ifelse(home_odds < away_odds, 1, 0)))

glimpse(dataB)

# make all columns integers

data2 <- dataB %>%
  mutate(extra_time.x = as.factor(ifelse(is.na(extra_time.x), 0, extra_time.x))) %>%
  mutate(days_since_home.y = ifelse(is.na(days_since_home.y), 0, days_since_home.y )) %>%
  mutate(offset_games_since_home_match.x = ifelse(is.na(offset_games_since_home_match.x), 0, offset_games_since_home_match.x)) %>%
  mutate(rankDifference = ifelse(is.na(rankDifference), 0, rankDifference)) %>%
  filter(matchcounter.x >= 10 & !is.na(favourite) & rankDifference != 0) %>% # remove the first 9 games from every team for every season 
  select(-c("opponent_team", "target_team", "original_game", "date", "target_goals.x", "opponent_goals.x",
            "target_goal_difference.x", "home_odds", "away_team", "tie_odds", "away_odds", "target_homeAway.x",
            "matchcounter.x")) %>%
  filter(season.x != "2019/2020") %>%
  mutate_if(is.numeric, as.integer, na.rm = FALSE) %>%
  mutate_if(is.integer, scaling_0_100)

table(data2$season.x)

# correlation plot & splitting----------------------------------------------------------------------------
glimpse(data2)
sum(is.na(data2))

cor_matrix <- data2 %>% select(-c(1:3, 36)) %>% cor()

cor_plot <- cor_matrix %>% 
  corrplot()

# remove highly correlated features

data3 <- data2 %>%
  select(-c("V16.x", "V18.x", "V16.y", "V18.y", "offset_season_points.y", "V20.x", "V22.x",
            "V20.y", "V22.y", "offset_losses_since_win.x", "offset_losses_since_win.y",
            "offset_games_since_home_match.x", "offset_games_since_home_match.y",
            "seasonPointsDifference", "V17.x", "V17.y", "V21.x", "V21.y", "days_since_home.x"))

# rename some volumns

data4 <- data3 %>% 
  rename(
    five_game_performance_target = V19.x,
    five_game_plusminus_target = V23.x,
    five_game_performance_opponent = V19.y,
    five_game_plusminus_opponent = V23.y
  )

glimpse(data4)

cor_matrix2 <- data4 %>% select(-c(1:3,17)) %>% cor()

cor_plot <- cor_matrix2 %>% 
  corrplot()

cor_matrix2B <- cor_matrix2[cor_matrix2 < 1]

max(cor_matrix2B)
min(cor_matrix2B)



training <- data4 %>%
  filter(season.x != "2018/2019") %>%
  select(-"season.x")
  
names(training)

testing <- data4 %>%
  filter(season.x == "2018/2019") %>%
  select(-"season.x")

testing_labels <- testing$target_outcome.x

testing <- testing %>% select(-"target_outcome.x")

# MARS Model ---------------------------------------------------------------------------------------

fitControl <- trainControl(
  method = "repeatedcv",
  repeats = 10, 
  number = 10)

set.seed(825)

# MARS MODEL
grid = expand.grid( degree = 1, nprune = 40)

mars1 = train(target_outcome.x ~ .
                    , data = training
                    , method = 'earth'
                    , tuneGrid = grid
                    , trControl = trainControl(method = 'cv',
                                               verboseIter = FALSE,
                                               savePredictions = TRUE)
                    )

# mars model
predictions_mars1 <- predict(mars1, testing)
confusionMatrix(predictions_mars1, testing_labels)

vip(mars1, num_features = 5, geom = 'col', 
    aesthetics = list(col = "darkblue", fill = "white")) +
    labs(title = "Variable Importance") +
  theme_classic()





# Random Forest Model -------------------------------------------------------------------------

n_preds <- length(setdiff(names(training), "target_outcome.x"))

# train a default model

data_rf1 <- ranger(
  target_outcome.x ~ ., 
  data = training,
  mtry = floor(n_preds / 3),
  seed = 123
)

data_rf1$confusion.matrix

# get accuracy
rf1_accuracy <- (data_rf1$confusion.matrix[1, 1] + data_rf1$confusion.matrix[2, 2])/sum(data_rf1$confusion.matrix)
rf1_accuracy


# tuning hyperparametres

hyper_grid <- expand.grid(
  mtry = floor(n_preds * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA,
  num.trees = seq(50, 500, 50)
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = target_outcome.x ~ ., 
    data            = training, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
    importance = 'impurity'
  )
  # export test accuracy
  predictions_rf1 <- predict(fit, testing)
  matrix <- table(predictions_rf1$predictions, testing_labels)
  hyper_grid$testaccuracy[i] <- (matrix[1, 1] + matrix[2, 2])/sum(matrix)

}

# assess top 10 models
hyper_grid %>%
  arrange(desc(testaccuracy)) %>%
  head(10) # besta ccuracy is 58.54%

# Best Model

best_fit <- ranger(
  formula         = target_outcome.x ~ ., 
  data            = training, 
  num.trees       = 50,
  mtry            = 3,
  min.node.size   = 1,
  replace         = TRUE,
  sample.fraction = 0.63,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order',
  importance = 'impurity'
)


predictions_rf1 <- predict(best_fit, testing)
confusionMatrix(predictions_rf1$predictions, testing_labels)

vip(best_fit, num_features = 25, geom = 'col', 
    aesthetics = list(col = "darkblue", fill = "white")) +
  labs(title = "Variable Importance") +
  theme_classic()
