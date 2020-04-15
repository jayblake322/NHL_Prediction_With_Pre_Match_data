# Script 1 - Load, pre-process data including predictor creation

# all glimpse, View abd other data checks are commented out for easy sourcing of this file.

library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plyr)
library(purrr)
library(geosphere)
library(dplyr)

# # Load Data ---------------------------------------------------------------------------------
var_types <- c("date", "text", "numeric", "text", "numeric", "text", "numeric", "date")

nhlMatches <- read_excel("NHL Match Data 2014 to Early 2019 Regular Seasons Only.xlsx",
                         sheet = 1, 
                         col_types = var_types)

stadiums <- read.csv("teams.csv")
#glimpse(stadiums)

# Inspect Data --------------------------------------------------------------------------------

#summary(nhlMatches) # LOG has about 20% missing values 
#glimpse(nhlMatches)
# fix classes

nhlMatches2 <- nhlMatches %>%
  mutate(Visitor = as.factor(Visitor)) %>%
  mutate(Home = as.factor(Home)) 
 # select(-c("LOG", `Extra Time`)) # drop log

#glimpse(nhlMatches2)

# check levels
#(visitors <- levels(nhlMatches2$Visitor))
#(home <- levels(nhlMatches2$Home))
#levels(nhlMatches2$`Extra Time`)

# check all visitors and home are equal
#sum(visitors %in% home)
#sum(home %in% visitors)
#sum(home == visitors)
# all are equal

# imput missing values in 'ExtraTime' to be normal time match
nhlMatches2$`Extra Time`[is.na(nhlMatches2$`Extra Time`)] <- "Normal"

# change it to a facotr
nhlMatches3 <- nhlMatches2 %>%
  mutate(ExtraTime = as.factor(`Extra Time`)) %>%
  select(-c("LOG", `Extra Time`)) 

#glimpse(nhlMatches3)
#levels(nhlMatches3$ExtraTime)

# Outcome supervised classification label Creation and inspection --------------------------------------------------------------------------

# create the goal difference variable
nhlMatches3$goal_difference <- nhlMatches3$G...5 - nhlMatches3$G...3

# check the dispersion of results
#table(nhlMatches3$goal_difference)
#hist(nhlMatches3$goal_difference)


# create match outcome label BASED ON HOME TEAM
nhlMatches3$outcome <- ifelse(nhlMatches3$goal_difference >= 0, "Win", "Loss")

# inspect the distribution of home wins vs visitor wins
#table(nhlMatches3$outcome)
# there are more home wins than losses by approximately 600 games
#sum(nhlMatches3$outcome == "Win")/nrow(nhlMatches3) # 54.6% home wins.


# Split the dataframes for each team per match ------------------------------------------------

# rename scoring variables

names(nhlMatches3)[names(nhlMatches3) == "G...5"] <- "teamScore"
names(nhlMatches3)[names(nhlMatches3) == "G...3"] <- "opponentScore"

# duplicate dataframe 
nhlMatches3Duplicate <- nhlMatches3
nhlMatches3Duplicate$homeAway <- "Away"
nhlMatches3$homeAway <- "Home"


# recalculate the outcome for the away dataframe
nhlMatches3Duplicate$outcome <- ifelse(nhlMatches3Duplicate$outcome == "Win", "Loss", "Win")


#table(nhlMatches3$outcome)
#table(nhlMatches3Duplicate$outcome)


# merge dataframes to have data according to each team per game


names(nhlMatches3Duplicate)
dup_names <- c("date", "targetTeam", "targetGoals", "opponent", "opponentGoals", "attendance", 
               "extraTime", "goal_difference", "outcome", "homeAway")
ori_names <- c("date", "opponent", "opponentGoals", "targetTeam", "targetGoals", "attendance", 
               "extraTime", "goal_difference", "outcome", "homeAway")

#glimpse(nhlMatches3Duplicate)
names(nhlMatches3Duplicate) <- dup_names
names(nhlMatches3) <- ori_names

nhlMatches4 <- nhlMatches3 %>%
  select(1, 4:5, 2:3, 6:10)

nhlMatches5 <- rbind(nhlMatches4, nhlMatches3Duplicate)

#glimpse(nhlMatches5)

nhlMatches6 <- nhlMatches5 %>%
  mutate(outcome = as.factor(outcome)) %>%
  mutate(homeAway = as.factor(homeAway))

#glimpse(nhlMatches6)

#table(nhlMatches5$outcome)


# create season predictor  -----------------------------

#create season variable
nhlMatches7 <- nhlMatches6%>%
  mutate(season = as.factor(ifelse(month(date) > 6, 
                         paste(year(date), "/", year(date) + 1, sep = ""),
                         paste(year(date) - 1, "/", year(date), sep = ""))))

#glimpse(nhlMatches7)
#levels(nhlMatches7$season)

# check home away proportions by year

homeAwayWinSummary <- nhlMatches7 %>%
  filter(homeAway == "Home") %>%
  group_by(season, outcome) %>%
  summarise(n = n()) %>%
  spread(outcome, n) %>%
  mutate(propWin = Win/sum(Win, Loss))
  
#plot(homeAwayWinSummary$propWin ~ homeAwayWinSummary$season,
 #    ylim = c(0, 1))
# not much variation each year. All years have more wins at home but only by a small margin indicating limited influence on the outcome..
       
# adjust goal_difference predictor
nhlMatches7$goal_difference <- nhlMatches7$targetGoals - nhlMatches7$opponentGoals
#glimpse(nhlMatches7)

# Create Points Predictor ------------------------------------------------------------

# create points received variable
#levels(nhlMatches7$extraTime)
#names(nhlMatches7)

nhlMatches7$points <- ifelse(nhlMatches7$outcome == "Win", 2,
                             ifelse(nhlMatches7$outcome == "Loss" & nhlMatches7$extraTime != "Normal", 1, 0)) 
  

# check result
#table(nhlMatches7$points)

# arrange by oldest to newest then split nhl matches by team 

nhlMatches7 <- nhlMatches7 %>% arrange(date)

team_scores <- split(nhlMatches7, list(nhlMatches7$targetTeam, nhlMatches7$season))
# Remove any empty data frames.... there are 3
team_scores <- team_scores[sapply(team_scores, function(x) dim(x)[1]) > 0]

no_lists <- length(team_scores)

# create season points cummulative sum variable
team_scores2 <- lapply(team_scores, function(df) {df$seasonPoints <- cumsum(df$points); df})

# check results
#View(team_scores2[[44]])
# plot(team_scores2[[55]]$seasonPoints, ylim = c(0, 120), main = team_scores2[[55]]$targetTeam[1])


# Create Predictors for Rolling n Match Point Streaks -----------------------------------------

# load rollingcumsum function
source("RollingCumSumOffset_Function.R")


# iterate over 1-5 game streak. Each dataframe in the list must have played atleast this many games. 2019/2020 has minimal games played

team_scores3 <- team_scores2

for (i in 1:183){
  
  ncol <- ncol(team_scores3[[i]]) + 1
  
  for (j in 1:5){
    
    
    team_scores3[[i]][, ncol] <- rollingCumSumOffset(team_scores3[[i]], "points", j)
    ncol <- ncol + 1
    
  }
}

# check results
#glimpse(team_scores3[[13]])

# Do the same for goal difference as a measure of how good or bad the losses have been on this streak

team_scores4 <- team_scores3

for (i in 1:183){
  
  ncol <- ncol(team_scores4[[i]]) + 1
  
  for (j in 1:5){
    
    
    team_scores4[[i]][, ncol] <- rollingCumSumOffset(team_scores4[[i]], "goal_difference", j)
    ncol <- ncol + 1
    
  }
}

# check the result
#glimpse(team_scores4[[183]])

# give names to all the variables

team_scores5 <- team_scores4

for (i in 1:183){
  
  team_scores5[[i]] <- team_scores5[[i]] %>%
    rename(
      OneGameStreak = V14,
      TwoGameStreak = V15,
      ThreeGameStreak = V16,
      FourGameStreak = V17,
      FiveGameStreak = V18,
      plusMinus1Games = V19,
      plusMinus2Games = V20,
      plusMinus3Games = V21,
      plusMinus4Games = V22,
      plusMinus5Games = V23
    )
}

# check result
# glimpse(team_scores5[[183]])

# Create number of days from various variable ----------------------------------------------------

# this section will create the number of away games and days since the last home game

# number of games since home

#View(team_scores5[[2]])

source("ResetCumSum_Function.R")


# create loss counter
team_scores6 <- lapply(team_scores5, function(df) {df$losscounter <- ifelse(df$outcome == "Win", 0, 1); df})
# cumsum reset for total losses since win
team_scores7 <- lapply(team_scores6, function(df) {df$lossesSinceWin <- resetCumSum(df$losscounter, 0); df})

#glimpse(team_scores7[[1]])

# Create days since last game predictor -------------------------------------------------------


daysSinceLastGame <- function(df){
  
  days <- vector()
  days[1] <- 0
  for (i in 2:nrow(df)){
  
    days[i] <- df$date[i] - df$date[i - 1]
  
  }

  return(days)
  
}  
  

team_scores7B <- lapply(team_scores7, function(df) {df$daysSinceGame <- daysSinceLastGame(df); df})

# check result
#glimpse(team_scores7B[[77]])                       


# Create games since last home game -----------------------------------------------------------

# create loss counter
team_scores8 <- lapply(team_scores7B, function(df) {df$awayCounter <- ifelse(df$homeAway == "Home", 0, 1); df})
# cumsum reset for total losses since win
team_scores9 <- lapply(team_scores8, function(df) {df$GamesSinceHomeMatch <- resetCumSum(df$awayCounter, 0); df})

# check result
#glimpse(team_scores9[[66]])

# Create days since home game predictor ---------------------------------------------------


source("NumberDaysSinceEvent_Function.R")

team_scores10 <- lapply(team_scores9, function(df) {df$daysSinceHome <- numberDaysSinceEvent(df,"homeAway", "date", "Home", "Away", 20, 0); df})

# check result
#glimpse(team_scores10[[7]])


# Create Distance Travelled Predictor  ---------------------------------------------------------

#plot(stadiums$Longitutde, stadiums$Latitude)

# create all combinations of stadiums
stadiumCombinations <- expand.grid(stadiums$Team, stadiums$Team, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)

# join stadium information to lhs and rhs stadium combination
stadiumCombinationsComplete <- stadiumCombinations %>%
  left_join(stadiums, by = c("Var1" = "Team")) %>%
  left_join(stadiums, by = c("Var2" = "Team"))
  

names(stadiumCombinationsComplete) <- c("targetTeam", "opponent", "targetCity", "targetStadium", "targetLat", "targetLon",
                                        "opponentCity", "opponentStadium", "opponentLat", "opponentLon")

#glimpse(stadiumCombinationsComplete)

#create distance dataframe for each combination
stadiumCombinationsComlete2 <- stadiumCombinationsComplete %>% rowwise() %>% 
  mutate(Distance = round(distHaversine(c(targetLon, targetLat), c(opponentLon, opponentLat))/1000,3))

# check result
#glimpse(stadiumCombinationsComlete2)

# custome function to calculate vector of last away game teams 
lastAwayTeam <- function(df){

  lastaAwayTeamVector <- vector()
  
  for (i in 2:nrow(df)){
  
  x <- i - 1

  if (df$homeAway[i] == "Home"){
  
    lastaAwayTeamVector[i] <- as.character(df$targetTeam[1])
    
    } else {
  
    lastaAwayTeamVector[i] <- as.character(df$opponent[x]) 
    
    
    }
  }
  
  return(lastaAwayTeamVector)
  
}

# apply function to create variable for all dataframes
team_scores10A <- lapply(team_scores10, function(df) {df$lastAwayTeam <- lastAwayTeam(df); df})

# check result
#glimpse(team_scores10A[[10]])

# set first trip for each dataframe so that it can be calculated in the distance of first road trip
team_scores10B <- lapply(team_scores10A, function(df) {df$lastAwayTeam[1] <- as.character(df$targetTeam[1]); df})

# check result
#glimpse(team_scores10B[[10]])


team_scores11 <- list()

# loop to jion distance calculations to each dataframe in the list
for (i in 1:length(team_scores10B)) {

  team_scores11[[i]] <- team_scores10B[[i]] %>%
  left_join(stadiumCombinationsComlete2, by = c("opponent" = "targetTeam", "lastAwayTeam" = "opponent")) %>%
  select(-c(30:31, 34:35))

}

#glimpse(team_scores11[[1]])
#names(team_scores11[[1]])
#warnings() # factor/character coercion

# check result
#glimpse(team_scores11[[4]])
#names(team_scores11[[55]])
# set first trip to 0 distance

source("CumSumResetColumnValue_Function.R")

# create cummulative distance predictor
team_scores12 <- lapply(team_scores11, function(df) {df$DistanceSinceHome <- cumSumResetOtherColumnValue(df,"homeAway", "Distance", "Home"); df})

# check result
#(team_scores12[[183]]$DistanceSinceHome)
#glimpse(team_scores12[[12]])


# creat augmented distance since home function to account for trip back home for first home match after a road trip
distanceFromHome2 <- function(df){

  y <- 1
  DistanceSinceHome2 <- vector()


  for (i in 2:nrow(df)){
  
    x <- i - 1
  
    if (df$DistanceSinceHome[i] == 0 & df$DistanceSinceHome[x] > 0){
    
      DistanceSinceHome2[i] <- df$DistanceSinceHome[x] + df$Distance[i]
      y <- i
  
    } else {
      
      DistanceSinceHome2[i] <- df$DistanceSinceHome[i]
  
    }
  }

  
  DistanceSinceHome2[1] <- ifelse(df$homeAway[1] == "Away", df$Distance[1], 0)
  return(DistanceSinceHome2)  
  
}


distanceFromHome2(team_scores12[[1]])

team_scores12A <- lapply(team_scores12, function(df) {df$roadTripDistance <- distanceFromHome2(df); df})


#glimpse(team_scores12A[[1]])


# Create Match Counter variable and total cumsum points  ---------------------------------------------------------------


# create match counter variable
team_scores13 <- lapply(team_scores12A, function(df) {df$matchcounter <- 1:nrow(df); df})

# check result
#glimpse(team_scores13[[18]])

# create cumsum plus minus predictor
team_scores14 <- lapply(team_scores13, function(df) {df$cumSumPlusMinus <- cumsum(df$goal_difference); df})

# check result
#glimpse(team_scores14[[3]])


# Create Ladder Position Predictor ------------------------------------------------------------

#glimpse(team_scores14[[1]])

# remerge all dataframes in the list

nhlMatches8 <- bind_rows(team_scores14)
#check result
#glimpse(nhlMatches8)

# split the dataframe by seasons
seasonMatches <- split(nhlMatches8, list(nhlMatches7$season))
# check resut
#dim(seasonMatches[[3]])


# create dataframe of every date in a season with every team as a column

seasonMatrices <- list()

for (z in 1:length(seasonMatches)){
  
  seasonx <- data.frame(date = unique(seasonMatches[[z]]$date))

  obs <- nrow(seasonx)
  names <- c("date", "home")

  for (i in 2:length(names)){

    x <- rep(NA, obs) # fill column with NAs
    seasonx$x <- x 
    seasonx <- seasonx %>% arrange(date)
    names(seasonx) <- names[1:i]

  }
  
  seasonMatrices[[z]] <- seasonx
}  

# check results
#View(seasonMatrices[[1]])

# add in each teams points for the season in their column against the right date.

#View(seasonMatches[[1]])


for (z in 1: length(seasonMatrices)){

  obs <- nrow(seasonMatches[[z]])
  
    for (i in 1:obs){
  
    # get score, date and team
    point <- seasonMatches[[z]]$points[i]
    team <- as.character(seasonMatches[[z]]$targetTeam[i])
    date <- seasonMatches[[z]]$date[i]
  
    # get row index of date
    index <- which(seasonMatrices[[z]]$date == date)  
  
    # put in the season dataframe
    seasonMatrices[[z]][index, team] <- point
    
  }
  
  seasonMatrices[[z]][is.na(seasonMatrices[[z]])] <- 0

}

# check result
#View(seasonMatrices[[5]])

# create a new dataframe of the cummulative result of seasonMatches
seasonMatricesCumSum <- list()

for (z in 1:length(seasonMatrices)){

  seasonDates <- seasonMatrices[[z]]$date 
  seasonCumSum <- as.data.frame(apply(seasonMatrices[[z]][, 2:32], 2, cumsum))
  seasonCumSum$date <- seasonDates
  seasonMatricesCumSum[[z]] <- seasonCumSum
  
}  

#View(seasonMatricesCumSum[[5]])
# create a new dataframe of ranks of each row so that each team is ranked on every date of the season

seasonDateRank <- list()

for (z in 1:length(seasonMatricesCumSum)){
  
  xranks <- as.data.frame(t(apply(-seasonMatricesCumSum[[z]][, -32], 1, rank, ties.method = 'min')))
  xranks$date <- seasonMatricesCumSum[[z]]$date
  seasonDateRank[[z]] <- xranks

}

# check result
#View(seasonDateRank[[1]])

# combine all into one

teamRanks <- rbind(seasonDateRank[[1]], seasonDateRank[[2]], seasonDateRank[[3]])
teamRanks2 <- rbind(seasonDateRank[[4]], seasonDateRank[[5]], seasonDateRank[[6]])

teamRanks$`Vegas Golden Knights` <- NA                   
teamRanks2$`Winnipeg Jets` <- NA

teamRanks <- teamRanks %>%
  select(1:31,33:32)

teamRanks2 <- teamRanks2 %>%
  select(1:29,31, 33, 30, 32)

teamRanks3 <- rbind(teamRanks, teamRanks2)


# Join ranks to main dataframe -------------------------------------------------------------------------

# gather team ranks dataframe

teamRanksGathered <- teamRanks3 %>% 
  gather(targetTeam, rank, -date)

# check results  
#glimpse(teamRanksGathered)
#glimpse(teamRanks3)
#glimpse(nhlMatches8)

# left join rank onto the main dataframe

MainDataFrameALL<- nhlMatches8 %>%
  left_join(teamRanksGathered, by = c("date" = "date", "targetTeam" = "targetTeam"))

# check result
#glimpse(MainDataFrameALL)
glimpse(MainDataFrameALL)

# Rearrange Main Dataframe to have target and opponent relevant predictors with "Outcome Ready for Models --------

# duplicate main dataframe
MainDataFrameAllDuplicate <- MainDataFrameALL

# rejoin to original with left join on opponent/targetteam and date

MainDataFrameFinal <- MainDataFrameALL %>%
  left_join(MainDataFrameAllDuplicate, by = c("opponent" = "targetTeam", "date" = "date"))

glimpse(MainDataFrameFinal)

#x is target team related and y is opponent team related in variable names



# Arrange dataframe for model ---------------------------------------------------------------------------------------------

# ratios, delete unwanted variables, differences etc


#glimpse(predictors)
#names(MainDataFrameFinal)

MainDataFrameFinal$rankDifference <- MainDataFrameFinal$rank.x - MainDataFrameFinal$rank.y
MainDataFrameFinal$seasonPointsDifference <- MainDataFrameFinal$seasonPoints.x - MainDataFrameFinal$seasonPoints.y
MainDataFrameFinal$seasonPlusMinusDifference <- MainDataFrameFinal$cumSumPlusMinus.x - MainDataFrameFinal$cumSumPlusMinus.y

# recode home and away predictor
MainDataFrameFinal$homeAway.x <- revalue(MainDataFrameFinal$homeAway.x, c("Home" = 1, "Away" = 0))
#glimpse(MainDataFrameFinal)

# Remove All Intermediary Variables and Data --------------------------------------------------

rm(homeAwayWinSummary)
rm(nhlMatches)
rm(nhlMatches2)
rm(nhlMatches3)
rm(nhlMatches3Duplicate)
rm(nhlMatches4)
rm(nhlMatches5)
rm(nhlMatches6)
rm(nhlMatches7)
rm(nhlMatches8)
rm(seasonCumSum)
rm(seasonDateRank)
rm(seasonMatrices)
rm(seasonMatches)
rm(seasonMatricesCumSum)
rm(seasonx)
rm(stadiumCombinations)
rm(stadiumCombinationsComlete2)
rm(stadiumCombinationsComplete)
rm(stadiums)
rm(team_scores)
rm(team_scores10)
rm(team_scores10A)
rm(team_scores10B)
rm(team_scores11)
rm(team_scores12)
rm(team_scores12A)
rm(team_scores13)
rm(team_scores14)
rm(team_scores2)
rm(team_scores3)
rm(team_scores4)
rm(team_scores5)
rm(team_scores6)
rm(team_scores7)
rm(team_scores7B)
rm(team_scores8)
rm(team_scores9)
rm(teamRanks)
rm(xranks)
rm(teamRanksGathered)
rm(date)
rm(dup_names)
rm(i)
rm(index)
rm(j)
rm(names)
rm(ncol)
rm(no_lists)
rm(obs)
rm(ori_names)
rm(point)
rm(seasonDates)
rm(team)
rm(var_types)
rm(x)
rm(z)
rm(teamRanks2)
rm(teamRanks3)
rm(MainDataFrameAllDuplicate)

write.csv(MainDataFrameFinal, "ModelReadyData.csv")
write.csv(MainDataFrameALL, "MainDataFrameReadyData.csv")

