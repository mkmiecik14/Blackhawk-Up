# NHL API
# Matt Kmiecik
# Started 13 July 2019

library(httr)
library(jsonlite)
library(dplyr)

# See the following link for a great intro into NHL.com API calls
# https://gitlab.com/dword4/nhlapi/blob/master/stats-api.md

# Function to retrive api data and clean
nhl_api <- function(link){
  
  GET(url = link) %>% content(as = "text") %>% fromJSON()
  
}

# Useage example:

# Blackhawks 2018-2019 schedule (doing this to get game ID)
# schedule_link <- "https://statsapi.web.nhl.com/api/v1/schedule?teamId=16&startDate=2018-10-01&endDate=2019-07-01"
# schedule_data <- nhl_api(schedule_link) # retrieves schedule data
# games_data <- schedule_data$dates$games # grabs games data
# games_data_flat <- map_dfr(games_data, jsonlite::flatten) # flattens nested dfs
# hawks_games <- games_data_flat$gamePk # THESE ARE THE HAWKS GAME IDs FOR 2018-2019
