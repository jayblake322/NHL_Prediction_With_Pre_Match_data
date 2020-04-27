# join points to rank
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/Shiny/")
ranks <- read.csv("team_ranks_table.csv") 
points <- readr::read_csv("season_points.csv")

library(dplyr)

glimpse(ranks)
glimpse(points)


points2 <- points %>% gather("team", "Points", -"date")
glimpse(points2)

rank_points <- ranks %>%
  left_join(points2, by = c("Date" = "date", "Team" = "team")) %>%
  mutate(Date = lubridate::dmy(Date))

glimpse(rank_points)

write.csv(rank_points, "team_ranks_table.csv")

