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

# Live Feed
game_id <- 2017020001
game_link <- paste0(
  "https://statsapi.web.nhl.com/api/v1/game/", 
  game_id,
  "/feed/live"
  )
live_data <- nhl_api(link = game_link)

# IDs for all plays
all_plays <- live_data$liveData$plays$allPlays

# Isolates play ID for scoring plays
scoring_plays <- live_data$liveData$plays$scoringPlays

# FIX THE BELOW DATA FRAMES, AS THEY CONTAIN COLUMNS THAT ARE LISTS

# ABOUT
# this has the time and period
time_data <- all_plays$about %>% filter(eventId %in% scoring_plays) %>% as_tibble()

# RESULTS
# this will have shot type, strenth, description, game winning goal, empty net
desc_data <- all_plays$result[scoring_plays+1,] %>% 
  rownames_to_column(., var = "eventId") %>%
  as_tibble(.) %>%
  mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects

# COORDINATES
# Should provide the coordinates of the goal; however, the rowname seems 1 off from id
coord_data <- all_plays$coordinates[scoring_plays+1,] %>%
  rownames_to_column(., var = "eventId") %>%
  as_tibble() %>%
  mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects

# TEAM
# Data about the team that scored
team_data <- all_plays$team[scoring_plays+1,] %>% 
  rownames_to_column(., var = "eventId") %>%
  as_tibble(.) %>%
  mutate(eventId = as.numeric(eventId), eventId = eventId - 1) # corrects
  
scoring_data <- bind_cols(time_data, desc_data, coord_data, team_data)


