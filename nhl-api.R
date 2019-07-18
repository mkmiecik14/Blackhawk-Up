# NHL API
# Matt Kmiecik
# Started 13 July 2019

library(httr)
library(jsonlite)
library(tidyverse)
library(RColorBrewer)


# https://gitlab.com/dword4/nhlapi/blob/master/stats-api.md#game-ids

# Function to retrive api data and clean
nhl_api <- function(link){
  
  GET(url = link) %>% content(as = "text") %>% fromJSON()
  
}

# Blackhawks 2018-2019 schedule (doing this to get game ID)
schedule_link <- "https://statsapi.web.nhl.com/api/v1/schedule?teamId=16&startDate=2018-10-01&endDate=2019-07-01"
schedule_data <- nhl_api(schedule_link) # retrieves schedule data
games_data <- schedule_data$dates$games # grabs games data
games_data_flat <- map_dfr(games_data, jsonlite::flatten) # flattens nested dfs
hawks_games <- games_data_flat$gamePk # THESE ARE THE HAWKS GAME IDs FOR 2018-2019

# Live Feed #

# Assembles links for api cals
game_links <- hawks_games %>% 
  map(function(x) paste0("https://statsapi.web.nhl.com/api/v1/game/", x,"/feed/live"))

# Using these above links, the api makes the calls for all 82 games
game_list <- game_links %>% map(~ nhl_api(.x))

# Extracts play data
all_live_data <- game_list %>%
  map("liveData") %>%
  map("plays") %>%
  map("allPlays") %>%
  map_dfr(~ .x %>% select(-players) %>% jsonlite::flatten(), .id = "game_number")

# Filter to only goals
all_goals <- all_live_data %>% 
  filter(result.event == "Goal") %>% 
  as_tibble() %>%
  mutate(
    about.periodTime = as.numeric(gsub(":", ".", about.periodTime)),
    Team = ifelse(team.name == "Chicago Blackhawks", "Blackhawks", "NHL")
    )
   
# Filtering to only look at regulation goals
all_reg_goals <- all_goals %>% filter(about.period %in% c(1:3))


rdgy_pal <- brewer.pal(11, "RdGy") # color palette

ggplot(
  all_reg_goals, 
  aes(about.periodTime, group = Team, color = Team)
  ) +
  geom_density() +
  geom_rug(alpha = 1/3) +
  scale_color_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Period Time", y = "Density", title = "Scoring Density Across Each Period") +
  facet_wrap(~about.period) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Game-winning goals
ggplot(
  all_reg_goals, 
  aes(about.periodTime, group = Team, color = Team)
  ) +
  geom_density() +
  geom_rug(alpha = 1/3) +
  scale_color_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Period Time", y = "Density") +
  facet_grid(result.gameWinningGoal~about.period) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Tieing the game
ggplot(
  all_reg_goals %>% filter(about.goals.away == about.goals.home),
  aes(about.periodTime, group = Team, color = Team)
  ) +
  geom_density() +
  geom_rug(alpha = 1/3) +
  scale_color_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Period Time", y = "Density", title = "Scoring Density to Tie the Game") +
  facet_wrap(~about.period) +
  theme_minimal() +
  theme(legend.position = "bottom")


shot_type_counts <- all_reg_goals %>% group_by(Team) %>% count(result.secondaryType)

ggplot(shot_type_counts %>% filter(n>1), aes(result.secondaryType, n, group = Team, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Shot Type", y = "Frequency", title = "Regulation Shot Totals per Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

all_reg_goals %>% count(Team)

# OT
all_ot_goals <- all_goals %>% filter(about.period == 4)

ggplot(
  all_ot_goals,
  aes(about.periodTime, group = Team, color = Team)
  ) +
  geom_density() +
  geom_rug(alpha = 1/3) +
  scale_color_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Period Time", y = "Density", title = "Overtime Scoring Density") +
  facet_wrap(~about.period) +
  theme_minimal() +
  theme(legend.position = "bottom")

all_ot_goals %>% count(Team)


# https://community.rapidminer.com/discussion/44904/using-the-nhl-api-to-analyze-pro-ice-hockey-data-part-1

# Shot maps
blue_line <- geom_vline(xintercept = 25, color = "blue")
red_line <- geom_vline(xintercept = 89, color = "red")
red_dot_1 <- geom_polygon() # 5 feet behind blue line and 22 feet from center

ggplot(
  all_reg_goals,
  aes(sqrt(coordinates.x^2), coordinates.y, group = Team, color = result.secondaryType)
  ) +
  blue_line + red_line +
  geom_point(aes(shape = Team)) +
  #facet_wrap(~result.secondaryType) +
  theme_classic() +
  theme(legend.position = "bottom")

