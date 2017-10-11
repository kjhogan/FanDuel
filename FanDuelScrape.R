workspace_Init <- function() {
  #set up workspace
  packages <- #need all of these installed including some from github
    c('tidyverse',
      'rvest',
      'purrr',
      'stringr',
      'lubridate',
      'ggplot2')
  options(warn = -1)
  lapply(packages, library, character.only = T)
}

get_FD_Daily <- function(date) {
  #Get data for one day by date

  date <- as.Date(date, "%m/%d/%Y")
  print(paste0("Getting data for: ", date))
  base_link <- "http://rotoguru1.com/cgi-bin/hyday.pl?game=fd"
  page_link <- paste0(base_link,"&mon=",month(date),"&day=",day(date),"&year=",year(date))
  Sys.sleep(2)
  page <- read_html(page_link)
  
  stats_df <- page %>% html_nodes("table") %>% .[9] %>% html_table(fill = TRUE) %>% 
    as.data.frame(., stringsAsFactors = FALSE) %>% 
    filter (X1 %in% c('PG', 'SG', 'SF', 'PF', 'C'))
  
  if(nrow(stats_df) == 0) {
    print("No data for date")
    return(-1)
  }
  else {
    stats_df$gamedate <- date
    stats_df$cleanname <- gsub("\\^", '', stats_df$X2)
    stats_df$X4<- gsub("\\$|\\,", '', stats_df$X4)
    stats_df$cleanname <- sub("(\\w+),\\s(\\w+)", "\\2 \\1", stats_df$cleanname)
    colnames <- c('position', 'name', 'fdpts', 'salary', 'team', 'matchup', 'score', 'mins', 'line', 'date', 'cleanname' )
    names(stats_df) <- colnames
    stats_df <- stats_df %>% mutate(salary = as.numeric(salary), location = if_else(grepl("@", matchup), "Away", "Home"))
    return(stats_df)
  }
  
}


get_FD_Between <- function(startdate, enddate) {
  gamedates <- seq.Date(as.Date(startdate, "%m/%d/%Y"), as.Date(enddate, "%m/%d/%Y"), "day")
  all_data <- lapply(gamedates, get_FD_Daily)
  non_empty_day <- all_data %>% map_lgl(is.data.frame)
  all_data <- all_data[non_empty_day]
  all_data <- bind_rows(all_data)
  return(all_data)
}

