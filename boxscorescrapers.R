get_trad_boxscores <- function( LeagueID = "00",
                                Season = "2020-21",
                                SeasonType = "Regular Season",
                                PlayerOrTeam = "T",
                                Counter = "1000",
                                Sorter = "DATE",
                                Direction = "DESC",
                                DateFrom = "",
                                DateTo = ""
                                 
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
  params = formals(get_trad_boxscores)

  
  response <- httr::GET(url = 'https://stats.nba.com/stats/leaguegamelog', httr::add_headers(.headers = headers), query = params, content_type_json()) 
  
  json <- jsonlite::fromJSON(content(response, "text"))
  
  df <- purrr::pluck(json, "resultSets", "rowSet", 1) %>% as_tibble()
  colnames(df) <- purrr::pluck(json, "resultSets", "headers") %>% unlist()
  df <- df %>% janitor::clean_names()
  df <- df %>% mutate_at(vars(min:plus_minus), as.numeric)
  df$team_name[df$team_name == "LA Clippers"] <- "Los Angeles Clippers"
  
  return(df)
  
}


