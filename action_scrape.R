
date <- Sys.Date()
# base url for game ids (from Canzhi)
root_url_ids <- "https://api.actionnetwork.com/mobile/v1/scoreboard/nba?date="

# combine base url with date - tan url needs hyphens removed
url_ids <- paste0(root_url_ids, date %>% str_remove_all("-"))

# get json from api
json_ids <- fromJSON(url_ids)


# grab json_ids, games, id
game_ids <- purrr::pluck(json_ids, "games")



# start with first game to figure out how to clean data
game_id <- game_ids[3]
odds <- game_ids$odds %>% bind_cols()