# Load Libraries ----------------------------------------------------------

library(nbastatR)
library(tidyverse)
library(janitor)
library(magrittr)
library(future)

date <- Sys.Date()

# get existing box scores - filter out games that are on the same day as props (in case these somehow get in)
#box_scores_existing <- read_rds("box_scores.rds") %>% filter(date_game < date)

# if the last game is more than one day before the date, we want to update box_scores so we have latest data for the model
#if(max(box_scores_existing$date_game) < date - 1){
  # Get Schedule and IDs for box_scores scrape ------------------------------
  # use date leftover from get_props.R to filter schedule
  schedule <- nbastatR::seasons_schedule(seasons = 2021, season_types = c("Regular Season")) %>%
    filter(dateGame < date)
  
  
  
  game_ids <- schedule %>% dplyr::select(idGame) %>% unique() %>% as_vector()
  
  
  # Get Game Logs -----------------------------------------------------------
  game_logs <- nbastatR::game_logs(seasons = 2021, league = "NBA", result_types = "player", season_types = c("Regular Season")) %>%
    filter(dateGame < date) %>% 
    clean_names()
  
  
  # Get box_scores
  box_scores <- nbastatR::box_scores(game_ids = game_ids, box_score_types = "traditional", result_types = "player", assign_to_environment = FALSE) %>% 
    clean_names() %>% 
    dplyr::select(data_box_score) %>% 
    unnest(data_box_score) %>%
    clean_names()
  
  
  # Join on game, team, and player id
  # game_logs on left, only want starter position, is_starter, and exact minutes from box_score
  box_join_vars <- box_scores %>% dplyr::select(id_game, id_team, id_player, group_start_position, is_starter, min_exact)
  
  box_logs <- left_join(game_logs, box_join_vars, by = c("id_game", "id_team", "id_player"))
  
  
  # join box new scraped
  box_logs_all <- bind_rows(box_scores_existing, box_logs)
  box_scores <- box_logs_all
  rm(box_logs_all, box_logs, box_join_vars, game_logs, game_ids, schedule, df_nba_player_dict)
#} 

#else {
 # box_scores <- box_scores_existing
#}

rm(box_scores_existing, get_props)
saveRDS(box_scores, "box_scores.rds")