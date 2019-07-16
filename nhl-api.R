# NHL API
# Matt Kmiecik
# Started 13 July 2019

library(httr)
library(jsonlite)
library(tidyverse)


# https://gitlab.com/dword4/nhlapi/blob/master/stats-api.md#game-ids
# API Link
api_base <- "https://statsapi.web.nhl.com"
api_link <- "https://statsapi.web.nhl.com/api/v1/teams"

# Function to retrive api data and clean
nhl_api <- function(link){
  
  GET(url = link) %>% content(as = "text") %>% fromJSON()
  
}

# Blackhawks 2018-2019 schedule (doing this to get game ID)
schedule_link <- "https://statsapi.web.nhl.com/api/v1/schedule?teamId=16&startDate=2018-10-01&endDate=2019-07-01"

schedule_data <- nhl_api(schedule_link) # retrieves schedule data

games_data <- schedule_data$dates$games # grabs games data
games_data_flat <- map_dfr(games_data, jsonlite::flatten) # flattens nested dfs

# THESE ARE THE HAWKS GAME IDs FOR 2018-2019
hawks_games <- games_data_flat$gamePk

# Live Feed #

# Assembles links for api cals
game_links <- hawks_games %>% 
  map(function(x) paste0("https://statsapi.web.nhl.com/api/v1/game/", x,"/feed/live"))

# Using these above links, the api makes the calls for all 82 games
game_list <- game_links %>% map(~ nhl_api(.x))

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
    about.periodTime = as.numeric(gsub(":", ".", all_goals$about.periodTime)),
    team = ifelse(team.name == "Chicago Blackhawks", "Blackhawks", "NHL")
    )
   

library(RColorBrewer)
rdgy_pal <- brewer.pal(11, "RdGy")
ggplot(
  all_goals %>% filter(about.period %in% c(1:3)), 
  aes(about.periodTime, group = team, color = team)
  ) +
  geom_density() +
  geom_rug(alpha = 1/3) +
  scale_color_manual(values = c(rdgy_pal[3], rdgy_pal[9])) +
  labs(x = "Period Time", y = "Density") +
  facet_wrap(~about.period) +
  theme_minimal() +
  theme(legend.position = "bottom")





# game_id <- 2017020001
# game_link <- paste0(
#   "https://statsapi.web.nhl.com/api/v1/game/", 
#   game_id,
#   "/feed/live"
#   )
# live_data <- nhl_api(link = game_link)
# 
# # IDs for all plays
# all_plays <- live_data$liveData$plays$allPlays
# 
# # Isolates play ID for scoring plays
# scoring_plays <- live_data$liveData$plays$scoringPlays
# 
# # Gathers data
# scoring_data <- all_plays %>% 
#   select(-players) %>% 
#   jsonlite::flatten(.) %>% # flattens nested dataframes
#   filter(about.eventIdx %in% scoring_plays) # filters to goals











# time_data <- jsonlite::flatten(all_plays$about) %>% 
#   filter(eventId %in% scoring_plays) %>% 
#   as_tibble()
# 
# # RESULTS
# # this will have shot type, strenth, description, game winning goal, empty net
# desc_data <- jsonlite::flatten(all_plays$result[scoring_plays+1,]) %>% 
#   rownames_to_column(., var = "eventId") %>%
#   as_tibble(.) %>%
#   mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects
# 
# # COORDINATES
# # Should provide the coordinates of the goal; however, the rowname seems 1 off from id
# coord_data <- jsonlite::flatten(all_plays$coordinates[scoring_plays+1,]) %>%
#   rownames_to_column(., var = "eventId") %>%
#   as_tibble() %>%
#   mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects
# 
# # TEAM
# # Data about the team that scored
# team_data <- jsonlite::flatten(all_plays$team[scoring_plays+1,]) %>% 
#   rownames_to_column(., var = "eventId") %>%
#   as_tibble(.) %>%
#   mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects
#   
# scoring_data <- bind_cols(time_data, desc_data, coord_data, team_data)


