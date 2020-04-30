
# set working directory
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/")

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(XLConnect)
library(scales)

# load data
data <- read.csv("df_engineered.csv") %>% select(-1) %>% # drop index 
  mutate(date = ymd(date))

wb <- loadWorkbook("Existing_nhl_scores.xlsx")
odds <- readWorksheet(wb, sheet = "odds", header = TRUE) %>%
  mutate(date = as.Date(date))

data <- data %>% left_join(odds, by = c("date" = "date", "target_team" = "home_team")) %>%
  mutate(favourite = as.factor(ifelse(home_odds < away_odds, "favourite", "underdog")))

# inspect data
glimpse(data)


# Home Team Advantage -------------------------------------------------------------------------

table(data$season.x, data$target_outcome.x) 

season_summary <- data %>%
  group_by(season.x, target_outcome.x) %>%
  summarise(number = n()) %>%
  spread(target_outcome.x, number) %>%
  mutate(prop_win = Win/sum(Loss, Win))

season_summary  

home_advantage <- ggplot(season_summary, aes(season.x, prop_win, fill = "")) +
  geom_bar(stat = "identity", fill = "white", col = "dark blue") +
  labs(title = "Home Ground Advantage",
       x = "Season",
       y = "Proportion of Wins") +
  geom_text(aes(y = prop_win, label = paste(round(prop_win * 100, 1), "%", sep = "")),
            vjust = 0.5, hjust = 1.2, 
            color = "black", size = 3.5) +
  theme_classic() + 
  geom_hline(yintercept = 0.5, linetype = "dashed", col = "red") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels = percent) +
  coord_flip()

home_advantage

# Rank Difference -----------------------------------------------------------------------------

season_summary_2 <- data %>%
  filter(!is.na(rankDifference)) %>%
  group_by(season.x, target_outcome.x) %>%
  summarise(number = n(), avg_rank_diff = mean(rankDifference)) %>%
  spread(target_outcome.x, number) %>%
  mutate(Loss_Win = ifelse(is.na(Loss), "Win", "Loss")) %>%
  select(1:2, 5) %>%
  spread(Loss_Win, avg_rank_diff)

season_summary_2

rank_dif <- ggplot(season_summary_2, aes(season.x, Loss, fill = "")) +
  geom_bar(stat = "identity", fill = "red", col = "black", alpha = 0.8) +
  geom_bar(stat = "identity", aes(season.x, Win), fill = "darkblue", col = "black", alpha = 0.8) +
  labs(title = "Winner and Loser Ranks",
       y = "Average Rank Difference",
       x = "Season") +
  geom_text(aes(y = Loss, label = round(Loss, 2)),
            vjust = -0.5, hjust = 0.5, 
            color = "white", size = 3.5) +
  geom_text(aes(y = Win, label = round(Win, 2)),
            vjust = 1, hjust = 0.5, 
            color = "white", size = 3.5) +
  annotate(geom = "text", x = "2014/2015", y = -2.2, label = "Losers",
           color="red") +
  annotate(geom = "text", x = "2014/2015", y = 2.2, label = "Winners",
           color="darkblue") +
  theme_classic()

rank_dif

win_rank_difference <- data %>% 
  filter(target_outcome.x == "Win", rankDifference > 0) %>%
  group_by(season.x) %>%
  summarise(number = n())

total_win_games <- data %>%
  filter(target_outcome.x == "Win") %>%
  group_by(season.x) %>%
  summarise(number = n())

win_rank_difference <- win_rank_difference %>%
  left_join(total_win_games, by = c("season.x" = "season.x")) %>%
  mutate(win_prop = number.x/number.y)

rank_difference <- ggplot(win_rank_difference, aes(season.x, win_prop, fill = "")) +
  geom_bar(stat = "identity", fill = "white", col = "dark blue") +
  labs(title = "Higher Rank Wins?",
       x = "Season",
       y = "Proportion of Wins") +
  geom_text(aes(y = win_prop, label = paste(round(win_prop * 100, 1), "%", sep = "")),
            vjust = 0.5, hjust = 1.2, 
            color = "black", size = 3.5) +
  theme_classic() + 
  geom_hline(yintercept = 0.5, linetype = "dashed", col = "red") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels = percent) +
  coord_flip()

rank_difference

# Favourite -----------------------------------------------------------------------------------

season_summary_3 <- data %>%
  filter(!is.na(favourite)) %>%
  group_by(season.x, target_outcome.x, favourite) %>%
  summarise(number = n()) %>%
  spread(target_outcome.x, number) %>%
  mutate(total_games = Loss + Win) %>%
  mutate(prop_win = Win/total_games) %>%
  select(1:2, 6) 
  spread(favourite, prop_win)



favourite <- ggplot(season_summary_3, aes(season.x, prop_win, fill = favourite)) +
                geom_bar(stat = "identity", position = position_dodge()) +
                labs(title = "Target Team Favourite and Underdog Game Outcomes",
                  y = "Win Proportion",
                  x = "Season") +
  annotate(geom = "text", x = "2018/2019", y = 0.35, label = "58%                     43%",
           color="black") +
  annotate(geom = "text", x = "2019/2020", y = 0.35, label = "56%                     44%",
           color="black") +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 0.5, linetype = "dashed", col = "red") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels = percent) +
  theme_classic()

  
favourite                




# Home, Favourite and Higher Rank -------------------------------------------------------------

season_summary_4 <- data %>%
  filter(favourite == "favourite" & rankDifference > 0) %>%
  group_by(season.x, target_outcome.x) %>%
  summarise(number = n()) %>%
  spread(target_outcome.x, number) %>%
  mutate(prop_win = Win/sum(Win, Loss)) 

season_summary_4

# Hot Streak ----------------------------------------------------------------------------------

par(mfrow = c(2, 2))

hist(data$V16.x, main = "2 game streak")
hist(data$V17.x, main = "3 game steak")
hist(data$V18.x, main = "4 game streak")
hist(data$V19.x, main = "5 game streak")

data_summary_5 <- data %>%
  select(c("season.x", "V16.x", "V17.x", "V18.x", "V19.x")) %>%
  group_by(season.x) %>%
  summarise(number = n(), of_max_2_streak = mean(V16.x)/4, of_max_3_streak = mean(V17.x)/6,
            of_max_4_streak = mean(V18.x)/8, of_max_5_streak = mean(V19.x)/10)


data_summary_5  # appear normally distributed

# if on a 3 game streak... how often is the next game a win

data_summary_6 <- data %>%
  arrange(target_team, date) %>%
  select(c("season.x", "target_outcome.x", "V17.x")) %>%
  filter(V17.x == 6) %>% # streak is offset. a score of 6 means three wins in last 3 games. 
  group_by(season.x, target_outcome.x) %>%
  summarise(number = n()) %>%
  spread(target_outcome.x, number) %>%
  mutate(total_games = Loss + Win) %>%
  mutate(prop_win = Win/total_games)

data_summary_6

par(mfrow = c(1,1))

three_game_streak <- ggplot(data_summary_6, aes(season.x, prop_win, fill = "")) +
  geom_bar(stat = "identity", fill = "white", col = "dark blue") +
  labs(title = "Winning the 4th Game After a 3 Game Hot Streak...",
       x = "Season",
       y = "Proportion of Wins") +
  geom_text(aes(y = prop_win, label = paste(round(prop_win * 100, 1), "%", sep = "")),
            vjust = 0.5, hjust = 1.2, 
            color = "black", size = 3.5) +
  theme_classic() + 
  geom_hline(yintercept = 0.5, linetype = "dashed", col = "red") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels = percent) +
  coord_flip()

three_game_streak
