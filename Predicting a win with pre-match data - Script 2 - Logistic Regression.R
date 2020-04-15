# Script 2 - Logistic Regression Model

# libraries

# data manipulation
library(dplyr) 

# model
library(caret) 
library(rsample)
library(vip)

# visualisation
library(corrplot)
library(Amelia)




# source dataframe
#source("Predicting a win with pre-match data - Script 1 - load - preprocess - create predictors.R")
MainDataFrameFinal <- read.csv("ModelReadyData.csv")

# warnings() # Warnings are just coercion of joining column to character vector
glimpse(MainDataFrameFinal)

# Arrange dataframe for model ---------------------------------------------------------------------------------------------


glimpse(MainDataFrameFinal)
names(MainDataFrameFinal)
table(MainDataFrameFinal$matchcounter.x)

LogisticDataFrame <- MainDataFrameFinal %>%
 filter(matchcounter.x >= 10) %>%
 select(-c(1:8, 11:12, 24, 27, 30:36, 38, 41:50, 62, 65, 68:74, 76)) 

glimpse(LogisticDataFrame)
names(LogisticDataFrame)

# Visualiase Predictor correlation and missing values ------------------------------------------------------------

# view missing values
missmap(LogisticDataFrame, main = "missing values")

# remove na values as they are small in number and relate to rank which should not be estimated
complRows <- complete.cases(LogisticDataFrame)
LogisticDataFrame2 <- LogisticDataFrame[complRows, ] # very minimal missing values

LogDfCor <- cor(LogisticDataFrame2[, -c(1:2)])



corrplot(LogDfCor) # acceptable correlation levels except betweens season points. Remove seasonPoints.y and keep it for the target team

LogisticDataFrame3 <- LogisticDataFrame2[, -21]
glimpse(LogisticDataFrame3)

# Logistic Regression model....NON-Convergence -------------------------------------------------------------------

set.seed(123)  # for reproducibility

# split data

data_split <- initial_split(LogisticDataFrame3, prop = 0.75, strata = "outcome.x")
train <- training(data_split)
test <- testing(data_split)
names(train)

# apply model with cross validation

# create model with all predictors
NHLlogModel <- train(outcome.x ~ .,
                         data = train,
                         method = "glm",
                         family = "binomial",
                         maxit = 500,
                         trControl = trainControl(method = "cv", number = 10)
)

warnings() # model did not converge. Likely due to correlated streak predictors

# assess model
summary(NHLlogModel)

preds <- predict(NHLlogModel, test)
glimpse(preds)
confusionMatrix(
    data = factor(preds),
    reference = test$outcome.x
)

# 100% accuracy. Results unreliable. Re-try by creating streak predictors as difference to opponent





# Logistic regression attempt 2 ---------------------------------------------------------------


names(LogisticDataFrame3)

LogisticDataFrame3$pointdiff1game <- LogisticDataFrame3$OneGameStreak.x - LogisticDataFrame3$OneGameStreak.y
LogisticDataFrame3$pointdiff2game <- LogisticDataFrame3$TwoGameStreak.x - LogisticDataFrame3$TwoGameStreak.y
LogisticDataFrame3$pointdiff3game <- LogisticDataFrame3$ThreeGameStreak.x - LogisticDataFrame3$ThreeGameStreak.y
LogisticDataFrame3$pointdiff4game <- LogisticDataFrame3$FourGameStreak.x - LogisticDataFrame3$FourGameStreak.y
LogisticDataFrame3$pointdiff5game <- LogisticDataFrame3$FiveGameStreak.x - LogisticDataFrame3$FiveGameStreak.y

LogisticDataFrame3$plusminusdiff1game <- LogisticDataFrame3$plusMinus1Games.x - LogisticDataFrame3$plusMinus1Games.y
LogisticDataFrame3$plusminusdiff2game <- LogisticDataFrame3$plusMinus2Games.x - LogisticDataFrame3$plusMinus2Games.y
LogisticDataFrame3$plusminusdiff3game <- LogisticDataFrame3$plusMinus3Games.x - LogisticDataFrame3$plusMinus3Games.y
LogisticDataFrame3$plusminusdiff4game <- LogisticDataFrame3$plusMinus4Games.x - LogisticDataFrame3$plusMinus4Games.y
LogisticDataFrame3$plusminusdiff5game <- LogisticDataFrame3$plusMinus5Games.x - LogisticDataFrame3$plusMinus5Games.y

# drop original streak predictors

LogisticDataFrame4 <- LogisticDataFrame3 %>%
  select(-c(4:13, 21:30))

glimpse(LogisticDataFrame4)


logisticdf2cor <- cor(LogisticDataFrame4[, -c(1:2)])
corrplot(logisticdf2cor)

# need to drop some variables with higher correlation
par(mfrow = c(1, 1))
names(LogisticDataFrame4)

LogisticDataFrame5 <- LogisticDataFrame4 %>%
  select(-c(3, 5:7, 9:14, 16:17, 19:20, 22:25, 26:30))

names(LogisticDataFrame5)
logisticdf3cor <- cor(LogisticDataFrame5[, -1])
corrplot(logisticdf3cor)



# model attempt 2

set.seed(123)  # for reproducibility

# split data

data_split <- initial_split(LogisticDataFrame5, prop = 0.5, strata = "outcome.x")
train <- training(data_split)
test <- testing(data_split)
names(train)

# apply model with cross validation

# create model with all predictors
NHLlogModel2 <- train(outcome.x ~ .,
                     data = train,
                     method = "nb",
                     laplace = 0.01,
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
)

warnings()
# Store Model and Accuracy Results for Comparison Script

NHLlogModel2


preds2 <- predict(NHLlogModel2, test[, -1])
glimpse(preds2)
(preds2)
length(preds2)
length(test$outcome.x)
confusionMatrix(
  data = factor(preds2),
  reference = test$outcome.x
)


table(preds2)
LogisticDataFrame5$outcome.x

