
# set working directory
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

# Load Libraies
librar(dplyr)
library(stringr)
library(XLConnect)
library(lubridate)

# load functions
source("FUNCTION_download_html_data.R")
# also loads dplyr when run


# download data from website
website <- "https://checkbestodds.com/hockey-odds/archive-nhl/" # current season link
new_odds <- download_html_data(website)

# Process the dataframe

# begin by splitting on space
new_odds$match <- grepl('-', new_odds$X1)

new_odds$date = NA


# add date column in
for (x in 1:nrow(new_odds)){

  if (new_odds$match[x] == FALSE){
    
    date <- new_odds$X1[x]
    
  }
  
  new_odds$date[x] = date
  
}

# get home and away teams

teams <- as.data.frame(str_split_fixed(new_odds$X1, "-", 2))
new_odds$away_team <- str_trim(teams[, 2], "both") 
new_odds$home_team <- str_trim(as.data.frame(str_split_fixed(teams$V1, " ", 2))[, 2], "both")

# filter for amtches only and format dates

new_odds2 <- new_odds %>%
  filter(match == TRUE) %>%
  select(6, 8:7, 2:4) %>%
  mutate(date = as.Date(date, format("%d %B %Y"))) %>%
  mutate(date = date - 1) %>%
  mutate(date = ymd(date)) %>%
  mutate(X3 = as.double(X3))

names(new_odds2) <- c("date", "home_team", "away_team", "home_odds", "tie_odds", "away_odds")


# load existing odds
wb <- loadWorkbook("Existing_nhl_scores.xlsx")
existing_odds <- readWorksheet(wb, sheet = "odds", header = TRUE)

# get latest existing date
max_date <- max(existing_odds$date)

# filter for updates only
odds_update <- new_odds2 %>%
  filter(date > max_date)

# write new data to excel sheet
appendWorksheet(wb, odds_update, sheet = "odds")
saveWorkbook(wb)

