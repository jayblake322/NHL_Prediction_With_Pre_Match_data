# Naive Bayes & GLM Model

setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics
library(caret) 
library(vip)
library(corrplot)


# Load and Inpsect Data -----------------------------------------------------------------------

data <- read.csv("df_engineered.csv") %>% select(-1) # drop index

glimpse(data)

# make all columns integers

data2 <- data %>%
  mutate(extra_time.x = as.factor(ifelse(is.na(extra_time.x), 0, extra_time.x))) %>%
  mutate(days_since_home.y = ifelse(is.na(days_since_home.y), 0, days_since_home.y )) %>%
  mutate(offset_rank.y = ifelse(is.na(offset_rank.y), 0, offset_rank.y)) %>%
  mutate(offset_games_since_home_match.x = ifelse(is.na(offset_games_since_home_match.x), 0, offset_games_since_home_match.x)) %>%
  mutate(rankDifference = ifelse(is.na(rankDifference), 0, rankDifference)) %>%
  mutate(offset_rank.x = ifelse(is.na(offset_rank.x), 0, offset_rank.x)) %>%
  filter(matchcounter.x >= 10) %>% # remove the first 9 games from every team for every season 
  select(-c("opponent_team", "target_team", "original_game", "date", "target_goals.x", "opponent_goals.x",
            "target_goal_difference.x")) %>%
  mutate_if(is.numeric, as.integer, na.rm = FALSE) 



# correlation plot & splitting----------------------------------------------------------------------------
glimpse(data2)
sum(is.na(data2))

cor_matrix <- data2 %>% select(-c(1:4)) %>% cor()

cor_matrix
plot(cor_matrix)

cor_plot <- cor_matrix %>% 
  corrplot()


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


# Models ---------------------------------------------------------------------------------------

fitControl <- trainControl(
  method = "repeatedcv",
  repeats = 10, 
  number = 10)

set.seed(825)

# NAIVE BAYES
nb1 <- train(target_outcome.x ~ ., data = training, 
                 method = "nb",
                 trControl = fitControl)

# LOGISITIC REGRESSION GLM
glm1 <- train(target_outcome.x ~ ., data = training, 
             method = "glm",
             family = "binomial",
             trControl = fitControl)


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


# Naive bayes
predictions_nb1 <- predict(nb1, testing)
confusionMatrix(predictions_nb1, factor(testing_labels))

# Glm
predictions_glm1 <- predict(glm1, testing)
confusionMatrix(predictions_glm1, factor(testing_labels))


# mars model
predictions_mars1 <- predict(mars1, testing)
confusionMatrix(predictions_mars1, testing_labels)

vip(mars1, num_features = 10, geom = 'col')
