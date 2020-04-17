
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plyr)
library(purrr)
library(geosphere)
library(dplyr)
library(XLConnect)

# # Load Data and inspect ---------------------------------------------------------------------------------

# load existing odds
wb <- loadWorkbook("Existing_nhl_scores.xlsx")
matches <- readWorksheet(wb, sheet = 'matches', header = TRUE)
stadiums <- readWorksheet(wb, sheet = "stadiums", header = TRUE)

glimpse(matches)
glimpse(stadiums)

matches_2 <- matches %>%
  mutate(Visitor = as.factor(Visitor)) %>%
  mutate(Home = as.factor(Home)) %>%
  mutate(Extra.Time = as.factor(Extra.Time)) %>%
  mutate(G = as.integer(G)) %>%
  mutate(G.1 = as.integer(G.1)) %>%
  mutate(Att. = as.integer(Att.))

names(matches_2)[names(matches_2) == "G"] <- "away_score"
names(matches_2)[names(matches_2) == "G.1"] <- "home_score"

glimpse(matches_2)
summary(matches_2) 


# check levels
(visitors <- levels(matches_2$Visitor))
(home <- levels(matches_2$Home))
sum(home == visitors) # 100% match

table(matches_2$Extra.Time)

summary(stadiums)


# Outcome supervised classification label Creation and inspection --------------------------------------------------------------------------

# create the goal difference variable
matches_2$goal_difference <- matches_2$home_score - matches_2$away_score 

# check the dispersion of results
table(matches_2$goal_difference)
hist(matches_2$goal_difference) # looks normally distributed... no 0 goals because there are tie breakers


# create match outcome label BASED ON HOME TEAM
matches_2$home_outcome <- ifelse(matches_2$goal_difference >= 0, "Win", "Loss")

# inspect the distribution of home wins vs losses
table(matches_2$home_outcome) # 54.4% home wins

# Split the dataframes for each team per match ------------------------------------------------

# duplicate dataframe 
matches_2_dup <- matches_2
matches_2_dup$homeAway <- "Away"
matches_2$homeAway <- "Home"


# recalculate the outcome for the away dataframe
matches_3_dup <- matches_2_dup %>%
  mutate(away_outcome = ifelse(matches_2_dup$home_outcome == "Win", "Loss", "Win")) %>% 
  select(1:8, 11:10)

table(matches_2$home_outcome)
table(matches_3_dup$away_outcome)


# merge dataframes to have data according to each team per game

col_names <- c("date", "opponent_team", "opponent_goals", "target_team", "target_goals", "extra_time", 
               "attendance", "target_goal_difference", "target_outcome", "target_homeAway")

col_names_dup <- c("date", "target_team", "target_goals", "opponent_team", "opponent_goals", "extra_time", 
               "attendance", "target_goal_difference", "target_outcome", "target_homeAway")


names(matches_2) <- col_names
names(matches_3_dup) <- col_names_dup

matches_4_dup <- matches_3_dup %>% select(1, 4:5, 2:3, 6:10)

matches_3 <- rbind(matches_2, matches_4_dup) %>%
              mutate(target_homeAway = as.factor(target_homeAway)) %>%
              mutate(target_outcome = as.factor(target_outcome)) 

glimpse(matches_3)
table(matches_3$target_outcome)


# create season predictor  -----------------------------

#create season variable
matches_4 <- matches_3%>%
  mutate(season = as.factor(ifelse(month(date) > 6, 
                         paste(year(date), "/", year(date) + 1, sep = ""),
                         paste(year(date) - 1, "/", year(date), sep = "")))) %>%
  select(11, 1:10)

glimpse(matches_4)
levels(matches_4$season)
table(matches_4$season)

# check home away proportions by year

homeAwayWinSummary <- matches_4 %>%
  filter(target_homeAway == "Home") %>%
  group_by(season, target_outcome) %>%
  summarise(n = n()) %>%
  spread(target_outcome, n) %>%
  mutate(propWin = Win/sum(Win, Loss))
  
plot(homeAwayWinSummary$propWin ~ homeAwayWinSummary$season,
     ylim = c(0, 1))
      

# Create Standing Points Variable ------------------------------------------------------------

# create points received variable

matches_4$points <- ifelse(matches_4$target_outcome == "Win", 2,
                             ifelse(matches_4$target_outcome == "Loss" & !is.na(matches_4$extra_time), 1, 0)) 
  
table(matches_4$points)

# arrange by oldest to newest then split nhl matches by team 

matches_4 <- matches_4 %>% arrange(date)

team_matches <- split(matches_4, list(matches_4$target_team, matches_4$season))

# Remove any empty data frames.... there are 3 due to teams changes i.e. vegas knights didn't exist in 2014/15 season
team_matches <- team_matches[sapply(team_matches, function(x) dim(x)[1]) > 0]

length(team_matches)

# create season points cummulative sum variable
team_matches_2 <- lapply(team_matches, function(df) {df$season_points <- cumsum(df$points); df})

# check results
glimpse(team_matches_2[[44]])
plot(team_matches_2[[55]]$season_points, ylim = c(0, 120), main = team_matches_2[[55]]$target_team[1])


# Create Predictors for Rolling n Match Point Streaks -----------------------------------------

# load rollingcumsum function
source("FUNCTION_RollingCumSumOffset.R")

# iterate over 1-5 game streak. Each dataframe in the list must have played atleast this many games. 2019/2020 has minimal games played
# there is an offset to show what the streak was leading into the current game

team_matches_3 <- team_matches_2

for (i in 1:length(team_matches_3)){
  
  ncol <- ncol(team_matches_3[[i]]) + 1
  
  for (j in 2:5){ # 2 game steak to a 5 game streak
    
    
    team_matches_3[[i]][, ncol] <- rollingCumSumOffset(team_matches_3[[i]], "points", j)
    ncol <- ncol + 1
    
  }
}

# check results
glimpse(team_matches_3[[13]])

# Do the same for goal difference as a measure of how good or bad the losses have been on this streak

team_matches_4 <- team_matches_3

for (i in 1:length(team_matches_4)){
  
  ncol <- ncol(team_matches_4[[i]]) + 1
  
  for (j in 2:5){
    
    
    team_matches_4[[i]][, ncol] <- rollingCumSumOffset(team_matches_4[[i]], "target_goal_difference", j)
    ncol <- ncol + 1
    
  }
}

# check the result
glimpse(team_matches_4[[183]])
barplot(team_matches_4[[183]]$V21, main = "5 game streak plus minus example")

# give names to all the variables

for (i in 1:183){
  
  team_matches_4[[i]] <- team_matches_4[[i]] %>%
    rename(
      TwoGameStreak = V14,
      ThreeGameStreak = V15,
      FourGameStreak = V16,
      FiveGameStreak = V17,
      plusMinus2Games = V18,
      plusMinus3Games = V19,
      plusMinus4Games = V20,
      plusMinus5Games = V21
    )
}

# check result
glimpse(team_matches_4[[183]])

# Create number of days from various variable ----------------------------------------------------

# this section will create the number of away games and days since the last home game

# number of games since home

source("FUNCTION_ResetCumSum.R")

# create loss counter
team_matches_5 <- lapply(team_matches_4, function(df) {df$losscounter <- ifelse(df$target_outcome == "Win", 0, 1); df})

# cumsum reset for total losses since win
team_matches_5 <- lapply(team_matches_5, function(df) {df$lossesSinceWin <- resetCumSum(df$losscounter, 0); df})

glimpse(team_matches_5[[1]])
table(team_matches_5[[1]]$lossesSinceWin)

# Create days since last game predictor -------------------------------------------------------


daysSinceLastGame <- function(df){
  
  days <- vector()
  days[1] <- 0
  for (i in 2:nrow(df)){
  
    days[i] <- df$date[i] - df$date[i - 1]
  
  }

  return(days)
  
}  
  
team_matches_6 <- lapply(team_matches_5, function(df) {df$daysSinceGame <- daysSinceLastGame(df); df})

# check result
glimpse(team_matches_6[[77]])          

# Create games since last home game -----------------------------------------------------------

# create loss counter
team_matches_7 <- lapply(team_matches_6, function(df) {df$awayCounter <- ifelse(df$target_homeAway == "Home", 0, 1); df})
# cumsum reset for total losses since win
team_matches_7 <- lapply(team_matches_7, function(df) {df$games_since_home_match_1 <- resetCumSum(df$awayCounter, 0); df})


# check result
glimpse(team_matches_7[[66]])
barplot(team_matches_7[[66]]$games_since_home_match)



# Create days since home game predictor ---------------------------------------------------


source("FUNCTION_NumberDaysSinceEvent.R")

team_matches_8 <- lapply(team_matches_7, function(df) {df$days_since_home <- numberDaysSinceEvent(df,"target_homeAway", "date", "Home", "Away", 20, 0); df})

# check result
glimpse(team_matches_8[[7]])


# Create Distance Travelled Predictor  ---------------------------------------------------------

plot(stadiums$Longitutde, stadiums$Latitude)

# create all combinations of stadiums
stadium_combos <- expand.grid(stadiums$Team, stadiums$Team, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)

# join stadium information to lhs and rhs stadium combination
stadium_combos_complete <- stadium_combos%>%
  left_join(stadiums, by = c("Var1" = "Team")) %>%
  left_join(stadiums, by = c("Var2" = "Team"))
  

names(stadium_combos_complete) <- c("targetTeam", "opponent", "targetCity", "targetStadium", "targetLat", "targetLon",
                                        "opponentCity", "opponentStadium", "opponentLat", "opponentLon")

glimpse(stadium_combos_complete)

#create distance dataframe for each combination
stadium_combos_complete_2 <- stadium_combos_complete %>% rowwise() %>% 
  mutate(Distance = round(distHaversine(c(targetLon, targetLat), c(opponentLon, opponentLat))/1000,3))

# check result
glimpse(stadium_combos_complete_2)

# custome function to calculate vector of last away game teams 
lastAwayTeam <- function(df){

  lastAwayTeamVector <- vector()
  
  for (i in 1:nrow(df)){
  

  if (df$target_homeAway[i] == "Home"){
  
    lastAwayTeamVector[i] <- as.character(df$target_team[i])
    
    } else {
      
    lastAwayTeamVector[i] <- as.character(df$opponent_team[i]) 
    
    }
  }
  
  
  z <- length(lastAwayTeamVector)
  actual_vector <- rep(NA, z)
  
  
  for (i in 2:z){
    
    x <- i - 1
    actual_vector[i] <- lastAwayTeamVector[x]
    
  }
  
  return(actual_vector)
  
}


# apply function to create variable for all dataframes
team_matches_9 <- lapply(team_matches_8, function(df) {df$lastAwayTeam <- lastAwayTeam(df); df})

# check result
glimpse(team_matches_9[[10]])

# set first trip for each dataframe so that it can be calculated in the distance of first road trip
team_matches_10 <- lapply(team_matches_9, function(df) {df$lastAwayTeam[1] <- as.character(df$target_team[1]); df})

# check result
glimpse(team_matches_10[[10]])

team_matches_11 <- list()

# loop to join distance calculations to each dataframe in the list
for (i in 1:length(team_matches_10)) {

  team_matches_11[[i]] <- team_matches_10[[i]] %>%
  left_join(stadium_combos_complete_2, by = c("target_team" = "targetTeam", "lastAwayTeam" = "opponent")) %>%
  select(-c(29:30, 33:34))

}

glimpse(team_matches_11[[1]])
# warnings() # factor/character coercion

# readjust names of variables

for (i in 1:length(team_matches_11)){
  
  team_matches_11[[i]] <- team_matches_11[[i]] %>%
    rename(
      last_opponent_lat = opponentLat,
      last_opponent_lon = opponentLon
      
    )
}

glimpse(team_matches_11[[3]])

source("FUNCTION_CumSumResetColumnValue.R")

# create cummulative distance predictor
team_matches_12 <- lapply(team_matches_11, function(df) {df$DistanceSinceHome <- cumSumResetOtherColumnValue(df,"target_homeAway", "Distance", "Home"); df})

# check result
(team_matches_12[[3]]$DistanceSinceHome)


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

  
  DistanceSinceHome2[1] <- ifelse(df$target_homeAway[1] == "Away", df$Distance[1], 0)
  return(DistanceSinceHome2)  
  
}

team_matches_13 <- lapply(team_matches_12, function(df) {df$roadTripDistance <- distanceFromHome2(df); df})

glimpse(team_matches_13[[1]])


# Create Match Counter variable and total cumsum points  ---------------------------------------------------------------


# create match counter variable
team_matches_14 <- lapply(team_matches_13, function(df) {df$matchcounter <- 1:nrow(df); df})

# check result
glimpse(team_matches_14[[18]])

# create cumsum plus minus predictor
team_matches_14 <- lapply(team_matches_14, function(df) {df$cumSumPlusMinus <- cumsum(df$target_goal_difference); df})

# check result
glimpse(team_matches_14[[3]])


# REMERGE ##### Create Ladder Position Predictor ------------------------------------------------------------

# remerge all dataframes in the list

matches_5 <- bind_rows(team_matches_14)
#check result

glimpse(matches_5)
dim(matches_5)

# split the dataframe by seasons
season_matches <- split(matches_5, list(matches_5$season))

# check resut
dim(season_matches[[3]])


# create dataframe of every date in a season with every team as a column

seasonMatrices <- list()

for (z in 1:length(season_matches)){
  
  seasonx <- data.frame(date = unique(season_matches[[z]]$date))

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
View(seasonMatrices[[1]])

# add in each teams points for the season in their column against the right date.

for (z in 1: length(seasonMatrices)){

  obs <- nrow(season_matches[[z]])
  
    for (i in 1:obs){
  
    # get score, date and team
    point <- season_matches[[z]]$points[i]
    team <- as.character(season_matches[[z]]$target_team[i])
    date <- season_matches[[z]]$date[i]
  
    # get row index of date
    index <- which(seasonMatrices[[z]]$date == date)  
  
    # put in the season dataframe
    seasonMatrices[[z]][index, team] <- point
    
  }
  
  seasonMatrices[[z]][is.na(seasonMatrices[[z]])] <- 0

}

# check result
glimpse(seasonMatrices[[5]])

# create a new dataframe of the cummulative result of seasonMatches
seasonMatricesCumSum <- list()

for (z in 1:length(seasonMatrices)){

  seasonDates <- seasonMatrices[[z]]$date 
  seasonCumSum <- as.data.frame(apply(seasonMatrices[[z]][, 2:32], 2, cumsum))
  seasonCumSum$date <- seasonDates
  seasonMatricesCumSum[[z]] <- seasonCumSum
  
}  

glimpse(seasonMatricesCumSum[[5]])
# create a new dataframe of ranks of each row so that each team is ranked on every date of the season

seasonDateRank <- list()

for (z in 1:length(seasonMatricesCumSum)){
  
  xranks <- as.data.frame(t(apply(-seasonMatricesCumSum[[z]][, -32], 1, rank, ties.method = 'min')))
  xranks$date <- seasonMatricesCumSum[[z]]$date
  seasonDateRank[[z]] <- xranks

}

# check result
glimpse(seasonDateRank[[1]])

# combine all into one

teamRanks <- rbind(seasonDateRank[[1]], seasonDateRank[[2]], seasonDateRank[[3]])
teamRanks2 <- rbind(seasonDateRank[[4]], seasonDateRank[[5]], seasonDateRank[[6]])

glimpse(teamRanks)

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
  select(-"home") %>%
  gather(targetTeam, rank, -date)

table(teamRanksGathered$targetTeam)

# check results  
glimpse(teamRanksGathered)
glimpse(teamRanks3)

# left join rank onto the main dataframe

MainDataFrameALL <- matches_5 %>%
  left_join(teamRanksGathered, by = c("date" = "date", "target_team" = "targetTeam"))

# check result
glimpse(MainDataFrameALL)

# Rearrange Main Dataframe to have target and opponent relevant predictors with "Outcome Ready for Models --------

# duplicate main dataframe
MainDataFrameAllDuplicate <- MainDataFrameALL

# rejoin to original with left join on opponent/targetteam and date

MainDataFrameFinal <- MainDataFrameALL %>%
  left_join(MainDataFrameAllDuplicate, by = c("opponent_team" = "target_team", "date" = "date"))

glimpse(MainDataFrameFinal)

#x is target team related and y is opponent team related in variable names

# Arrange dataframe for model ---------------------------------------------------------------------------------------------

# ratios, delete unwanted variables, differences etc

names(MainDataFrameFinal)

MainDataFrameFinal$rankDifference <- MainDataFrameFinal$rank.x - MainDataFrameFinal$rank.y
MainDataFrameFinal$seasonPointsDifference <- MainDataFrameFinal$season_points.x - MainDataFrameFinal$season_points.y
MainDataFrameFinal$seasonPlusMinusDifference <- MainDataFrameFinal$cumSumPlusMinus.x - MainDataFrameFinal$cumSumPlusMinus.y

# recode home and away predictor
MainDataFrameFinal$target_homeAway.x <- revalue(MainDataFrameFinal$target_homeAway.x, c("Home" = 1, "Away" = 0))
glimpse(MainDataFrameFinal)



# Write to csv ---------------------------------------------------------------------------

# too big to write to excel workbook with XL connect
write.csv(MainDataFrameFinal, 'df_engineered.csv')

# Remove All Intermediary Variables and Data --------------------------------------------------

rm(homeAwayWinSummary)
rm(matches)
rm(matches_2)
rm(matches_3)
rm(matches_2_dup)
rm(matches_4)
rm(matches_3_dup)
rm(matches_5)
rm(matches_4_dup)
rm(season_matches)
rm(seasonCumSum)
rm(seasonDateRank)
rm(seasonMatrices)
rm(seasonMatricesCumSum)
rm(seasonx)
rm(stadium_combos)
rm(stadium_combos_complete)
rm(stadium_combos_complete_2)
rm(stadiums)
rm(team_matches)
rm(team_matches_10)
rm(team_matches_11)
rm(team_matches_12)
rm(team_matches_13)
rm(team_matches_14)
rm(team_matches_2)
rm(team_matches_3)
rm(team_matches_4)
rm(team_matches_5)
rm(team_matches_6)
rm(team_matches_7)
rm(team_matches_8)
rm(team_matches_9)
rm(teamRanks)
rm(teamRanks2)
rm(teamRanks3)
rm(xranks)
rm(teamRanksGathered)

rm(col_names_dup)
rm(col_names)
rm(index)
rm(j)
rm(i)
rm(names)
rm(ncol)
rm(obs)
rm(point)
rm(seasonDates)
rm(team)
rm(x)
rm(z)
rm(date)
rm(home)
rm(visitors)
rm(MainDataFrameAllDuplicate)
rm(MainDataFrameALL)

rm(cumSumResetOtherColumnValue)
rm(daysSinceLastGame)
rm(distanceFromHome2)
rm(lastAwayTeam)
rm(numberDaysSinceEvent)
rm(resetCumSum)
rm(rollingCumSumOffset)





