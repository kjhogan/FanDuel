
get_misc_stats <- function(
  MeasureType = "Misc",
  PerMode = "PerGame",
  PlusMinus = "N",
  PaceAdjust = "N",
  Rank = "N",
  LeagueID = "00",
  Season = "2020-21",
  SeasonType = "Regular Season",
  PORound = "0",
  Outcome = '',
  Location = '',
  Month = "0",
  SeasonSegment = '',
  DateFrom = '',
  DateTo = '',
  OpponentTeamID = "0",
  VsConference = '',
  VsDivision = '',
  TeamID = "0",
  Conference = '',
  Division = '',
  GameSegment = '',
  Period = "0",
  ShotClockRange = '',
  LastNGames = "0",
  GameScope = '',
  PlayerExperience = '',
  PlayerPosition = '',
  StarterBench = '',
  TwoWay = "0"
){

  require(tidyverse)
  require(jsonlite)
  require(httr)
  require(janitor)
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.116 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Sec-Fetch-Dest` = 'empty',
    `Referer` = 'https://www.nba.com',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  params = formals(get_misc_stats)
 
  
  response <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers = headers), query = params, content_type_json()) 

  json <- jsonlite::fromJSON(content(response, "text"))
  
  df <- purrr::pluck(json, "resultSets", "rowSet", 1) %>% as_tibble()
  colnames(df) <- purrr::pluck(json, "resultSets", "headers") %>% unlist()
  df <- df %>% janitor::clean_names()
  
  return(df)
}



get_advanced_stats <- function(  MeasureType = "Advanced",
                                 PerMode = "PerGame",
                                 PlusMinus = "N",
                                 PaceAdjust = "N",
                                 Rank = "N",
                                 LeagueID = "00",
                                 Season = "2020-21",
                                 SeasonType = "Regular Season",
                                 PORound =  "0",
                                 Outcome = "",
                                 Location = "",
                                 Month =  "0",
                                 SeasonSegment = "",
                                 DateFrom = "",
                                 DateTo = "",
                                 OpponentTeamID =  "0",
                                 VsConference = "",
                                 VsDivision = "",
                                 TeamID =  "0",
                                 Conference = "",
                                 Division = "",
                                 GameSegment = "",
                                 Period =  "0",
                                 ShotClockRange = "",
                                 LastNGames =  "0",
                                 GameScope = "",
                                 PlayerExperience = "",
                                 PlayerPosition = "",
                                 StarterBench = "",
                                 TwoWay =  "0"
  
) {
  require(tidyverse)
  require(jsonlite)
  require(httr)
  require(janitor)
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.116 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Sec-Fetch-Dest` = 'empty',
    `Referer` = 'https://www.nba.com',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  params = formals(get_advanced_stats)
  params$LastNGames[1] <- LastNGames
  
  response <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers = headers), query = params, content_type_json()) 
  
  json <- jsonlite::fromJSON(content(response, "text"))
  
  df <- purrr::pluck(json, "resultSets", "rowSet", 1) %>% as_tibble()
  colnames(df) <- purrr::pluck(json, "resultSets", "headers") %>% unlist()
  df <- df %>% janitor::clean_names()
  df <- df %>% mutate_at(vars(gp:cfid), as.numeric)
  df$team_name[df$team_name == "LA Clippers"] <- "Los Angeles Clippers"
  
  return(df)

}


get_table_oe <- function(teams = "") {
  require(gt)
  require(purrr)
  
  last_three <- get_advanced_stats(LastNGames = "3") %>% 
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L3_ORtg", "L3_DRtg", "L3_Pace")
  
  last_five <- get_advanced_stats(LastNGames = "5") %>% 
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L5_ORtg", "L5_DRtg", "L5_Pace")
  
  last_ten <- get_advanced_stats(LastNGames = "10") %>%
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L10_ORtg", "L10_DRtg", "L10_Pace")
  
  teams_list <- nbastatR::nba_teams() %>% filter(idLeague == "2") %>% 
    select(nameTeam, urlThumbnailTeam) %>%
    purrr::set_names("team", "thumbnail")
  
  rolling_summary <- last_three %>% left_join(last_five, by = "team") %>%
    left_join(last_ten, by = "team") %>%
    left_join(teams_list, by = "team")
 
  print(length(teams))
  
  if(length(teams) == 1){
  rolling_summary %<>% select(thumbnail,team, L3_ORtg, L5_ORtg, L10_ORtg)
  }
  else{
    rolling_summary %<>% select(thumbnail,team, L3_ORtg, L5_ORtg, L10_ORtg)
    rolling_summary %<>% filter(team %in% teams)
  }
  table_graphic <- rolling_summary %>%
    gt() %>%
    tab_header(
      title = md("**NBA Rolling Summary**"),
      subtitle = md(paste0("As of ", format(Sys.Date(), format = "%B %d, %Y")))
    ) %>%
    cols_label(`thumbnail` = (""),
               `team` = ("Team"),
               `L3_ORtg` = ("Off Rtg L3"),
               `L5_ORtg` = ("Off Rtg L5"),
               `L10_ORtg` = ("Off Rtg L10")) %>%
    text_transform(
      locations = cells_body(vars(`thumbnail`)),
      fn = function(x){
        web_image(url = x,
                  height = px(27.5))
      }
    ) %>%
    data_color(
      columns = vars(L10_ORtg),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = NULL
      )
    ) %>%
    gt_theme_538(table.width = px(550))
    
  return(table_graphic)
}

get_table_de <- function(teams = "") {
  require(gt)
  require(purrr)
  
  last_three <- get_advanced_stats(LastNGames = "3") %>% 
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L3_ORtg", "L3_DRtg", "L3_Pace")
  
  last_five <- get_advanced_stats(LastNGames = "5") %>% 
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L5_ORtg", "L5_DRtg", "L5_Pace")
  
  last_ten <- get_advanced_stats(LastNGames = "10") %>%
    select(team_name, off_rating, def_rating, pace) %>%
    purrr::set_names("team", "L10_ORtg", "L10_DRtg", "L10_Pace")
  
  teams_list <- nbastatR::nba_teams() %>% filter(idLeague == "2") %>% 
    select(nameTeam, urlThumbnailTeam) %>%
    purrr::set_names("team", "thumbnail")
  
  rolling_summary <- last_three %>% left_join(last_five, by = "team") %>%
    left_join(last_ten, by = "team") %>%
    left_join(teams_list, by = "team")
  
  print(length(teams))

  if(length(teams) == 1){
    rolling_summary %<>% select(thumbnail,team, L3_DRtg, L5_DRtg, L10_DRtg)
  }
  else{
    rolling_summary %<>% select(thumbnail,team, L3_DRtg, L5_DRtg, L10_DRtg)
    rolling_summary %<>% filter(team %in% teams)
  }
    
  table_graphic <- rolling_summary %>% 
    gt() %>%
    tab_header(
      title = md("**NBA Rolling Summary**"),
      subtitle = md(paste0("As of ", format(Sys.Date(), format = "%B %d, %Y")))
    ) %>%
    cols_label(`thumbnail` = (""),
               `team` = ("Team"),
               `L3_DRtg` = ("Def Rtg L3"),
               `L5_DRtg` = ("Def Rtg L5"),
               `L10_DRtg` = ("Def Rtg L10")) %>%
    text_transform(
      locations = cells_body(vars(`thumbnail`)),
      fn = function(x){
        web_image(url = x,
                  height = px(27.5))
      }
    ) %>%
    data_color(
      columns = vars(L10_DRtg),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = NULL
      )
    ) %>%
    gt_theme_538(table.width = px(550))
  
  return(table_graphic)
}


gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}