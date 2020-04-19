# Random Forest Model

setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics
library(ranger)   # a c++ implementation of random forest 
library(h2o) 
library(vip)


# Load and Inpsect Data -----------------------------------------------------------------------

data <- read.csv("df_engineered.csv") %>% select(-1) # drop index

glimpse(data)

# make all columns integers

data2 <- data %>%
  mutate_if(is.numeric, as.integer, na.rm = FALSE) %>%
  mutate(extra_time.x = as.factor(ifelse(is.na(extra_time.x), "Normal", extra_time.x))) %>%
  mutate(days_since_home.y = ifelse(is.na(days_since_home.y), 0, days_since_home.y )) %>%
  mutate(offset_rank.y = ifelse(is.na(offset_rank.y), 0, offset_rank.y)) %>%
  mutate(offset_games_since_home_match.x = ifelse(is.na(offset_games_since_home_match.x), 0, offset_games_since_home_match.x)) %>%
  mutate(rankDifference = ifelse(is.na(rankDifference), 0, rankDifference)) %>%
  mutate(offset_rank.x = ifelse(is.na(offset_rank.x), 0, offset_rank.x)) %>%
  filter(matchcounter.x >= 10) # remove the first 9 games from every team for every season 

training <- data2 %>%
  filter(season.x != "2019/2020") %>%
  select(c("rankDifference", "target_outcome.x", "V19.x", "V23.x", "roadTrip_cumsum.x",
           "V19.y", "V23.y", "roadTrip_cumsum.y"))

testing <- data2 %>%
   filter(season.x == "2019/2020") %>%
   select(c("rankDifference", "V19.x", "V23.x", "roadTrip_cumsum.x",
            "V19.y", "V23.y", "roadTrip_cumsum.y"))
         
testing_labels <- data2 %>%
   filter(season.x == "2019/2020") 
         
testing_labels <- testing_labels$target_outcome.x


# Model Preparation -------------------------------------------------------------------------

n_preds <- length(setdiff(names(training), "target_outcome.x"))

# train a default model

data_rf1 <- ranger(
  target_outcome.x ~ ., 
  data = training,
  mtry = floor(n_preds / 3),
  seed = 123
)

# Get OOb RMSE
(default_rmse <- sqrt(data_rf1$prediction.error))

# tuning hyperparametres

hyper_grid <- expand.grid(
  mtry = floor(n_preds * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = target_outcome.x ~ ., 
    data            = training, 
    num.trees       = n_preds * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
    importance = 'impurity'
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}


# assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)


data_rf1 <- ranger(
  formula         = target_outcome.x ~ ., 
  data            = training, 
  num.trees       = n_preds * 10,
  mtry            = 1,
  min.node.size   = 10,
  replace         = FALSE,
  sample.fraction = 0.5,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order',
  importance = 'impurity'
)

predict(data_rf1, testing_labels)

vip(data_rf1, num_features = 37, geom = 'col')

