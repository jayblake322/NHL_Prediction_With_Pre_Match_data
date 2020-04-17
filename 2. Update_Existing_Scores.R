
# set working directory
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")
# Load Libraries and Functions ----------------------------------------------------------------

# load libraries
library(lubridate)
library(stringr)
library(XLConnect)

# load functions
source("FUNCTION_download_html_data.R")
# also loads dplyr when run


# ETL -----------------------------------------------------------------------------------------


# connect to excel workbook and load existing scores
wb <- loadWorkbook("Existing_nhl_scores.xlsx")
existing_scores <- readWorksheet(wb, sheet = 'matches', header = TRUE)



# download data from website
current_year <- year(Sys.Date())
website <- paste("https://www.hockey-reference.com/leagues/NHL_", current_year, "_games.html", sep = '')
new_scores <- download_html_data(website)

# update existing data
max_date <- max(existing_scores$Date) # get latest existing match date

# names(new_scores)
# names(existing_scores)


# transform new data
new_scores_formatted <- new_scores %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Att. = str_remove(Att., ',')) %>%
  mutate(Att. = as.numeric(Att.)) %>%
  filter(Date > max_date) %>%
  filter(!is.na(G...3)) %>% # do not include games that have not been played yet
  mutate(...6 = ifelse(...6 %in% c("OT", "SO"), ...6 , NA)) %>%
  select(1:7) 

names(new_scores_formatted) <- names(existing_scores)

# write new data to excel sheet
appendWorksheet(wb, new_scores_formatted, sheet = 1)
saveWorkbook(wb)







