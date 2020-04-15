# Script 2B - Logistic Regression Model | Opponent data not joined to observation due to complete separation issues

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


# Load Data -----------------------------------------------------------------------------------

modelData <- read.csv("MainDataFrameReadyData.csv")

glimpse(modelData)


# Organise Dataframe for Model ----------------------------------------------------------------


names(modelData)
table(modelData$matchcounter)

LogisticDataFrame <- modelData %>%
  filter(matchcounter >= 10) %>%
  select(-c(1:3, 5, 7:8, 12, 31:36, 39)) 

glimpse(LogisticDataFrame)
names(LogisticDataFrame)

# recode home and away predictor
LogisticDataFrame$homeAway <- as.integer(revalue(LogisticDataFrame$homeAway, c("Home" = 1, "Away" = 0))) # 2 = home 1 = away

glimpse(LogisticDataFrame$homeAway)



# Visualise Data Correlations and Missing -----------------------------------------------------

# view missing values
missmap(LogisticDataFrame, main = "missing values") # less than 1% missing values and only in one column

complRows <- complete.cases(LogisticDataFrame)
LogisticDataFrame2 <- LogisticDataFrame[complRows, ]

# number of actual non-complete rows was...
nrow(LogisticDataFrame) - nrow(LogisticDataFrame2) # 148 out of 11100

# visualise correlation between predictors
glimpse(LogisticDataFrame2)
names(LogisticDataFrame2)
LogDfCor <- cor(LogisticDataFrame2[, -c(4, 18, 21)])

# visualise correlation plot
corrplot(LogDfCor) 


# identify highly correlated predictors and remove them
(highlyCorDescr <- findCorrelation(LogDfCor, cutoff = .75, exact = TRUE)) # correlation cutoff at absoulate 0.75

predictors <- LogisticDataFrame2[, -c(4, 18, 21)]
filteredDescr <- predictors[, -highlyCorDescr]
glimpse(filteredDescr)

cor(filteredDescr)
corrplot(cor(filteredDescr))



# Apply Logistic Regression Model -------------------------------------------------------------

logdf <- filteredDescr
logdf$outcome <- LogisticDataFrame2$outcome
glimpse(logdf)
set.seed(123)  # for reproducibility

# split data

data_split <- initial_split(logdf, prop = 0.75, strata = "outcome")
train <- training(data_split)
test <- testing(data_split)
names(train)

# apply model with cross validation

# create model with all predictors
NHLlogModel <- train(outcome ~ .,
                     data = train,
                     method = "glm",
                     family = "binomial",
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
)

warnings() # non-convergence still
table(MainDataFrameALL$outcome)

# assess model
summary(NHLlogModel)

##### KEEP OPPONENT RATIOS FOR EACH MATCH BT REMOVE THE OPPONENT ENTRY. THEREFORE THERE WILL BE DIFFERENT TOTALS OF WINS AND LOSSES 
