
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

# load libraries

library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plyr)
library(purrr)
library(geosphere)
library(dplyr)
library(XLConnect)

# load functions
source("FUNCTIONS_Feature_Engineering.R")
drop_column <- function(df, col){
  
  new_df <- df %>% select(-col)
  
  return(new_df)
} 
daysSinceLastGame <- function(df){
  
  days <- vector()
  days[1] <- 0
  for (i in 2:nrow(df)){
    
    days[i] <- df$date[i] - df$date[i - 1]
    
  }
  
  return(days)
  
} 
dayse_since_home_adjust <- function(df){
  
  x = nrow(df) - 1
  index = 1
  
  for (i in 2:x){
    
    
    if (df[i, "target_homeAway"] == "Home" & df[i - 1, "target_homeAway"] != "Home"){
      
      df[i, "days_since_home"] = df[i, "date"] - df[index, "date"]
      index = i
      
    } else if (df[i, "target_homeAway"] == "Home" & df[i + 1, "target_homeAway"] == "Away"){
      
      index <- i
    }
    
  }  
  
  return(df$days_since_home)
  
}
adjust_coords <- function(df){
  
  df <- df %>%
    mutate(opponentLat = ifelse(target_homeAway == "Home", targetLat, opponentLat)) %>%
    mutate(opponentLon = ifelse(target_homeAway == "Home", targetLon, opponentLon))
  
  return(df)
  
}
road_trip <- function(df) {
  z <- nrow(df)
  index <- 1
  lat_1 <- 0
  lat_2 <- 0
  lon_1 <- 0
  lon_2 <- 0
  
  df$road_trip_distance <- 0
  
  for (i in 2:z){
    
    if (df[i, "target_homeAway"] == "Away") {
      
      lat_1 <- df[i, "opponentLat"] 
      lon_1 <- df[i, "opponentLon"]
      lat_2 <- df[index, "opponentLat"] 
      lon_2 <- df[index, "opponentLon"]
      
      df[i, "road_trip_distance"] <- round(distHaversine(c(lon_1, lat_1), c(lon_2, lat_2))/1000,0)
      index <- i
      
    } else if (df[i, "target_homeAway"] == "Home" & df[i - 1, "target_homeAway"] == "Away") {
      
      lat_1 <- df[i, "opponentLat"] 
      lon_1 <- df[i, "opponentLon"]
      lat_2 <- df[index, "opponentLat"] 
      lon_2 <- df[index, "opponentLon"]
      
      df[i, "road_trip_distance"] <-  round(distHaversine(c(lon_1, lat_1), c(lon_2, lat_2))/1000,0)
      index <- i
      
    } else {
      
      index <- i
      
    }
    
  }
  
  return(df)
  
}


# # Load Data and inspect ---------------------------------------------------------------------------------

# load existing odds
wb <- loadWorkbook("Existing_nhl_scores.xlsx")
matches <- readWorksheet(wb, sheet = 'matches', header = TRUE)
stadiums <- readWorksheet(wb, sheet = "stadiums", header = TRUE)

glimpse(matches)
#glimpse(stadiums)

matches_2 <- matches %>%
  mutate(Visitor = as.factor(Visitor)) %>%
  mutate(Home = as.factor(Home)) %>%
  mutate(Extra.Time = as.factor(Extra.Time)) %>%
  mutate(G = as.integer(G)) %>%
  mutate(G.1 = as.integer(G.1)) %>%
  mutate(Att. = as.integer(Att.))

names(matches_2)[names(matches_2) == "G"] <- "away_score"
names(matches_2)[names(matches_2) == "G.1"] <- "home_score"

#glimpse(matches_2)
#summary(matches_2) 


# check levels
#(visitors <- levels(matches_2$Visitor))
#(home <- levels(matches_2$Home))
#sum(home == visitors) # 100% match

#table(matches_2$Extra.Time)

#summary(stadiums)

# Outcome supervised classification label Creation and inspection --------------------------------------------------------------------------

# create the goal difference variable
matches_2$goal_difference <- matches_2$home_score - matches_2$away_score 

# check the dispersion of results
#table(matches_2$goal_difference)
#hist(matches_2$goal_difference) # looks normally distributed... no 0 goals because there are tie breakers


# create match outcome label BASED ON HOME TEAM
matches_2$home_outcome <- ifelse(matches_2$goal_difference >= 0, "Win", "Loss")

# inspect the distribution of home wins vs losses
#table(matches_2$home_outcome) # 54.4% home wins

# Split the dataframes for each team per match ------------------------------------------------

# duplicate dataframe 
matches_2_dup <- matches_2
matches_2_dup$homeAway <- "Away"
matches_2$homeAway <- "Home"


# recalculate the outcome for the away dataframe
matches_3_dup <- matches_2_dup %>%
  mutate(away_outcome = ifelse(matches_2_dup$home_outcome == "Win", "Loss", "Win")) %>% 
  select(1:8, 11:10)

#table(matches_2$home_outcome)
#table(matches_3_dup$away_outcome)


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

#glimpse(matches_3)
#table(matches_3$target_outcome)


# create season predictor  -----------------------------

#create season variable
matches_4 <- matches_3%>%
  mutate(season = as.factor(ifelse(month(date) > 6, 
                         paste(year(date), "/", year(date) + 1, sep = ""),
                         paste(year(date) - 1, "/", year(date), sep = "")))) %>%
  select(11, 1:10)

#glimpse(matches_4)
#levels(matches_4$season)
#table(matches_4$season)

# check home away proportions by year

homeAwayWinSummary <- matches_4 %>%
  filter(target_homeAway == "Home") %>%
  group_by(season, target_outcome) %>%
  summarise(n = n()) %>%
  spread(target_outcome, n) %>%
  mutate(propWin = Win/sum(Win, Loss))
  
#plot(homeAwayWinSummary$propWin ~ homeAwayWinSummary$season,
     #ylim = c(0, 1))
      

# Create Standing Points Variable ------------------------------------------------------------

# create points received variable

matches_4$points <- ifelse(matches_4$target_outcome == "Win", 2,
                             ifelse(matches_4$target_outcome == "Loss" & !is.na(matches_4$extra_time), 1, 0)) 
  
#table(matches_4$points)

# arrange by oldest to newest then split nhl matches by team 

matches_4 <- matches_4 %>% arrange(date)

team_matches <- split(matches_4, list(matches_4$target_team, matches_4$season))

# Remove any empty data frames.... there are 3 due to teams changes i.e. vegas knights didn't exist in 2014/15 season
team_matches <- team_matches[sapply(team_matches, function(x) dim(x)[1]) > 0]

#length(team_matches)

# create season points cummulative sum variable
team_matches_2 <- lapply(team_matches, function(df) {df$season_points <- cumsum(df$points); df})
# offset season points and points
team_matches_2 <- lapply(team_matches_2, function(df) {df$offset_season_points <- offset_column(df, "season_points", 1); df})
team_matches_2 <- lapply(team_matches_2, function(df) {df$offset_points <- offset_column(df, "points", 1); df})

# check results
#glimpse(team_matches_2[[44]])
#plot(team_matches_2[[55]]$season_points, ylim = c(0, 120), main = team_matches_2[[55]]$target_team[1])


# Create Predictors for Rolling n Match Point Streaks -----------------------------------------

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
#glimpse(team_matches_3[[13]])

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
glimpse(team_matches_4[[18]])
#barplot(team_matches_4[[183]]$V21, main = "5 game streak plus minus example")

# give names to all the variables

for (i in 1:183){
  
  team_matches_4[[i]] <- team_matches_4[[i]] %>%
    rename(
      TwoGameStreak = V16,
      ThreeGameStreak = V17,
      FourGameStreak = V18,
      FiveGameStreak = V19,
      plusMinus2Games = V20,
      plusMinus3Games = V21,
      plusMinus4Games = V22,
      plusMinus5Games = V23
    )
}

# check result
#glimpse(team_matches_4[[183]])

# Create number of days from various variable ----------------------------------------------------

# this section will create the number of away games and days since the last home game

# create loss counter
team_matches_5 <- lapply(team_matches_4, function(df) {df$losscounter <- ifelse(df$target_outcome == "Win", 0, 1); df})

# cumsum reset for total losses since win
team_matches_5 <- lapply(team_matches_5, function(df) {df$lossesSinceWin <- resetCumSum(df$losscounter, 0); df})
team_matches_5 <- lapply(team_matches_5, function(df) {df$offset_losses_since_win <- offset_column(df, "lossesSinceWin", 1); df})

#glimpse(team_matches_5[[1]])
#table(team_matches_5[[1]]$lossesSinceWin)

# Create days since last game predictor -------------------------------------------------------

team_matches_6 <- lapply(team_matches_5, function(df) {df$daysSinceGame <- daysSinceLastGame(df); df})
team_matches_6 <- lapply(team_matches_6, function(df) {df <- drop_column(df, "lossesSinceWin"); df})
team_matches_6 <- lapply(team_matches_6, function(df) {df <- drop_column(df, "losscounter"); df})
# check result
#glimpse(team_matches_6[[3]])          

# Create games since last home game -----------------------------------------------------------

# create loss counter
team_matches_7 <- lapply(team_matches_6, function(df) {df$awayCounter <- ifelse(df$target_homeAway == "Home", 0, 1); df})
# cumsum reset for total losses since win
team_matches_7 <- lapply(team_matches_7, function(df) {df$games_since_home_match_1 <- resetCumSum(df$awayCounter, 0); df})
team_matches_7 <- lapply(team_matches_7, function(df) {df$offset_games_since_home_match <- offset_column(df, "games_since_home_match_1", 1); df})
team_matches_7 <- lapply(team_matches_7, function(df) {df <- drop_column(df, "games_since_home_match_1"); df})
team_matches_7 <- lapply(team_matches_7, function(df) {df <- drop_column(df, "awayCounter"); df})


# check result
#glimpse(team_matches_7[[66]])
#barplot(team_matches_7[[66]]$games_since_home_match)



# Create days since home game predictor ---------------------------------------------------


team_matches_8 <- lapply(team_matches_7, function(df) {df$days_since_home <- numberDaysSinceEvent(df,"target_homeAway", "date", "Home", "Away", 20, 0); df})
# account for first home game
team_matches_8 <- lapply(team_matches_8, function(df) {df$days_since_home <- dayse_since_home_adjust(df); df})

# check result
#glimpse(team_matches_8[[7]])


# Create Distance Travelled Predictor  ---------------------------------------------------------

# create all combinations of stadiums
stadium_combos <- expand.grid(stadiums$Team, stadiums$Team, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)

# join stadium information to lhs and rhs stadium combination
stadium_combos_complete <- stadium_combos%>%
  left_join(stadiums, by = c("Var1" = "Team")) %>%
  left_join(stadiums, by = c("Var2" = "Team"))
  

names(stadium_combos_complete) <- c("targetTeam", "opponent", "targetCity", "targetStadium", "targetLat", "targetLon",
                                        "opponentCity", "opponentStadium", "opponentLat", "opponentLon")

#glimpse(stadium_combos_complete)

# check result
#glimpse(stadium_combos_complete_2)

team_matches_9 <- list()

# loop to join distance calculations to each dataframe in the list
for (i in 1:length(team_matches_8)) {

  team_matches_9[[i]] <- team_matches_8[[i]] %>%
  left_join(stadium_combos_complete, by = c("target_team" = "targetTeam", "opponent_team" = "opponent")) %>%
  select(-c(28:29, 32:33))

}

#glimpse(team_matches_9[[1]])
# warnings() # factor/character coercion

# create cummulative distance predictor
team_matches_10 <- lapply(team_matches_9, function(df) {df <- adjust_coords(df); df})
team_matches_11 <- lapply(team_matches_10, function(df) {df <- road_trip(df); df})

#glimpse(team_matches_11[[5]])

# cumsum reset of road trip distance
team_matches_13 <- lapply(team_matches_11, function(df) {df$roadTrip_cumsum <- resetCumSum(df$road_trip_distance, 0); df})
team_matches_13 <- lapply(team_matches_13, function(df) {df <- drop_column(df, "road_trip_distance"); df})

#glimpse(team_matches_13[[1]])


# Create Match Counter variable and total cumsum points  ---------------------------------------------------------------


# create match counter variable
team_matches_14 <- lapply(team_matches_13, function(df) {df$matchcounter <- 1:nrow(df); df})

# check result
# glimpse(team_matches_14[[18]])

# create cumsum plus minus predictor
team_matches_14 <- lapply(team_matches_14, function(df) {df$cumSumPlusMinus <- cumsum(df$target_goal_difference); df})
team_matches_14 <- lapply(team_matches_14, function(df) {df$offset_cumsum_plusminus <- offset_column(df, "cumSumPlusMinus", 1); df})
team_matches_14 <- lapply(team_matches_14, function(df) {df <- drop_column(df, "cumSumPlusMinus"); df})


# check result
#glimpse(team_matches_14[[3]])


# REMERGE ##### Create Ladder Position Predictor ------------------------------------------------------------

# remerge all dataframes in the list

matches_5 <- bind_rows(team_matches_14)
#check result

#glimpse(matches_5)
#dim(matches_5)

# split the dataframe by seasons
season_matches <- split(matches_5, list(matches_5$season))

# check resut
#dim(season_matches[[3]])


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
#glimpse(seasonMatrices[[1]])

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
glimpse(seasonMatrices[[4]])

# create a new dataframe of the cummulative result of seasonMatches
seasonMatricesCumSum <- list()

for (z in 1:3){

  seasonDates <- seasonMatrices[[z]]$date 
  seasonCumSum <- as.data.frame(apply(seasonMatrices[[z]][, 3:32], 2, cumsum))
  seasonCumSum$date <- seasonDates
  seasonMatricesCumSum[[z]] <- seasonCumSum
  
}  

glimpse(seasonMatricesCumSum[[3]])

# repeat for seasons with 31 teams

seasonMatricesCumSum2 <- list()

for (z in 4:6){
  
  seasonDates <- seasonMatrices[[z]]$date 
  seasonCumSum <- as.data.frame(apply(seasonMatrices[[z]][, 3:33], 2, cumsum))
  seasonCumSum$date <- seasonDates
  seasonMatricesCumSum2[[z]] <- seasonCumSum
  
}  

# glimpse(seasonMatricesCumSum2[[6]])

# add into seaonMatricesCumSum

seasonMatricesCumSum[[4]] <- seasonMatricesCumSum2[[4]]
seasonMatricesCumSum[[5]] <- seasonMatricesCumSum2[[5]]
seasonMatricesCumSum[[6]] <- seasonMatricesCumSum2[[6]]

glimpse(seasonMatricesCumSum[[3]])

#write.csv(seasonMatricesCumSum[[6]], "6.csv")
# create a new dataframe of ranks of each row so that each team is ranked on every date of the season

seasonDateRank <- list()

for (z in 1:3){
  
  xranks <- as.data.frame(t(apply(-seasonMatricesCumSum[[z]][, -31], 1, rank, ties.method = 'min')))
  xranks$date <- seasonMatricesCumSum[[z]]$date
  seasonDateRank[[z]] <- xranks

}

# repeat for seasons with 31 teams

for (z in 4:6){
  
  xranks <- as.data.frame(t(apply(-seasonMatricesCumSum[[z]][, -32], 1, rank, ties.method = 'min')))
  xranks$date <- seasonMatricesCumSum[[z]]$date
  seasonDateRank[[z]] <- xranks
  
}

# check result
glimpse(seasonMatricesCumSum[[5]])
glimpse(seasonDateRank[[6]])

# combine all into one

teamRanks <- rbind(seasonDateRank[[1]], seasonDateRank[[2]], seasonDateRank[[3]]) 
teamRanks2 <- rbind(seasonDateRank[[4]], seasonDateRank[[5]], seasonDateRank[[6]])

glimpse(seasonDateRank[[6]])

glimpse(teamRanks)
glimpse(teamRanks2)

teamRanks <- teamRanks %>%
  mutate(`Vegas Golden Knights` = NA) %>%
  select(31, 1:28, 32, 29:30)

# glimpse(teamRanks)

teamRanks2 <- teamRanks2 %>%
  select(32, 1:31)

# glimpse(teamRanks2)

teamRanks3 <- rbind(teamRanks, teamRanks2)

#glimpse(teamRanks3)

# Join ranks to main dataframe -------------------------------------------------------------------------

# gather team ranks dataframe

#write.csv(teamRanks3, "rank_adjust.csv")
teamRanks3B <- readr::read_csv("rank_adjust.csv") %>% mutate(date = ymd(date))

#glimpse(teamRanks3B)

teamRanksGathered <- teamRanks3B %>% 
  gather(targetTeam, rank, -date)

#glimpse(teamRanksGathered)

#table(teamRanksGathered$targetTeam)
#write.csv(teamRanksGathered, "team_ranks_table.csv")
# check results  
glimpse(teamRanksGathered)

# left join rank onto the main dataframe
#glimpse(matches_5)
matches_5B <- matches_5 %>% mutate(date = as.Date(date))

MainDataFrameALL <- matches_5B %>%
  left_join(teamRanksGathered, by = c("date" = "date", "target_team" = "targetTeam"))

# check result
glimpse(MainDataFrameALL)

# Rearrange Main Dataframe to have target and opponent relevant predictors with "Outcome Ready for Models --------

# duplicate main dataframe
MainDataFrameAllDuplicate <- MainDataFrameALL

# rejoin to original with left join on opponent/targetteam and date

MainDataFrameFinal <- MainDataFrameALL %>%
  left_join(MainDataFrameAllDuplicate, by = c("opponent_team" = "target_team", "date" = "date"))

# glimpse(MainDataFrameFinal)

#x is target team related and y is opponent team related in variable names

# Arrange dataframe for model ---------------------------------------------------------------------------------------------

# ratios, delete unwanted variables, differences etc

# names(MainDataFrameFinal)
#glimpse(MainDataFrameFinal)
MainDataFrameFinal$seasonPointsDifference <- MainDataFrameFinal$offset_season_points.x - MainDataFrameFinal$offset_season_points.y
MainDataFrameFinal$seasonPlusMinusDifference <- MainDataFrameFinal$offset_cumsum_plusminus.x - MainDataFrameFinal$offset_cumsum_plusminus.y

# table(MainDataFrameFinal$target_homeAway.x)

# recode home and away predictor
MainDataFrameFinal$target_homeAway.x <- revalue(MainDataFrameFinal$target_homeAway.x, c("Home" = 1, "Away" = 0))
MainDataFrameFinal$target_homeAway.y <- revalue(MainDataFrameFinal$target_homeAway.y, c("Home" = 1, "Away" = 0))

glimpse(MainDataFrameFinal)


# Write to csv and final adjustments---------------------------------------------------------------------------

# drop coordinate columns
#names(MainDataFrameFinal)
MainDataFrameFinal2 <- MainDataFrameFinal %>%
  select(-c(8, 12:13, 15, 28:31, 36:46, 48, 61:64, 66))

names(MainDataFrameFinal2)

# make sure games only recorded once using target outcome correct

MainDataFrameFinal2$original_game <- ifelse((MainDataFrameFinal$target_goals.x - MainDataFrameFinal$opponent_goals.x) == MainDataFrameFinal$target_goal_difference.x, 1, 0)
MainDataFrameFinal2 <- MainDataFrameFinal2 %>% 
  filter(original_game == 1) %>%
  select(-45)

#table(MainDataFrameFinal2$target_homeAway.x)

#glimpse(MainDataFrameFinal2)
# adjust rank - split dataframe then remerge

#rank_adjust <- split(MainDataFrameFinal2, list(MainDataFrameFinal2$target_team, MainDataFrameFinal2$season.x))
# Remove any empty data frames.... there are 3 due to teams changes i.e. vegas knights didn't exist in 2014/15 season
#rank_adjust <-  rank_adjust[sapply(rank_adjust, function(x) dim(x)[1]) > 0]

#rank_adjust <- lapply(rank_adjust, function(df) {df$offset_rank.x <- offset_column(df, "rank.x", 1); df})
#rank_adjust <- lapply(rank_adjust, function(df) {df$offset_rank.y <- offset_column(df, "rank.y", 1); df})
#MainDataFrameFinal3 <- bind_rows(rank_adjust)

MainDataFrameFinal2$rankDifference <- -(MainDataFrameFinal2$rank.x - MainDataFrameFinal2$rank.y)
glimpse(MainDataFrameFinal2)


MainDataFrameFinal2 <- drop_column(MainDataFrameFinal2, "rank.x")
MainDataFrameFinal2 <- drop_column(MainDataFrameFinal2, "rank.y")

#glimpse(MainDataFrameFinal3)

# too big to write to excel workbook with XL connect
write.csv(MainDataFrameFinal2, 'df_engineered.csv')



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
rm(stadiums)
rm(team_matches)
rm(team_matches_10)
rm(team_matches_11)
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
rm(MainDataFrameAllDuplicate)
rm(MainDataFrameALL)

rm(cumSumResetOtherColumnValue)
rm(daysSinceLastGame)

rm(resetCumSum)
rm(rollingCumSumOffset)

rm(MainDataFrameFinal)
rm(dayse_since_home_adjust)
rm(drop_column)
rm(offset_column)
rm(rollingCumSum)
rm(rank_adjust)
rm(MainDataFrameFinal2)
rm(adjust_coords)
rm(road_trip)
rm(wb)
rm(numberDaysSinceEvent)

