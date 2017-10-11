get_DateOdds <- function(date) {
  
  library(reshape2)
  date <- as.Date(date, "%m/%d/%Y")
  spreadurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=" ,format(date, "%Y%m%d"))
  totalsurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/totals/?date=" ,format(date, "%Y%m%d"))
  message(spreadurl)
  message(totalsurl)
  dfspreads <- data.frame()
  dftotals <- data.frame()
  Sys.sleep(2)
  spreadpage <- read_html(spreadurl) %>% html_nodes('#booksData')
  totalspage <- read_html(totalsurl) %>% html_nodes('#booksData')
  
  if (length(spreadpage) > 0){
    teams <- spreadpage %>% html_nodes(".team-name") %>% html_text()
    opener <- spreadpage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
    pinn <- spreadpage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
    
    dfspreads <- data.frame(teams, stringsAsFactors = F)
    dfspreads$opp <- dfspreads$teams
    dfspreads$opp[seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[-seq(0,length(dfspreads$teams),2)]
    dfspreads$opp[-seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[seq(0,length(dfspreads$teams),2)]
    dfspreads$date <- date
    
    dfspreads <- transform(dfspreads, opener = colsplit(opener, pattern = "\\s", names=c("odds","juice"))) 
    dfspreads <- transform(dfspreads, pinn = colsplit(pinn, pattern = "\\s", names=c("odds","juice"))) 
    
    
    dfspreads$pinn.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$pinn.odds))
    dfspreads$opener.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$opener.odds ))
    
    
    dfspreads$opener.juice[is.na(dfspreads$opener.juice)] <- as.numeric("-110")
    dfspreads$pinn.juice[is.na(dfspreads$pinn.juice)] <- as.numeric("-110")
  }
  
  if (length(totalspage) > 0){
    teams2 <- totalspage %>% html_nodes(".team-name") %>% html_text()
    opener2 <- totalspage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
    pinn2 <- totalspage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
    
    dftotals <- data.frame(teams2, stringsAsFactors = F)
    dftotals$opp <- dftotals$teams
    dftotals$opp[seq(0,length(dftotals$opp),2)] <- dftotals$teams[-seq(0,length(dftotals$teams2),2)]
    dftotals$opp[-seq(0,length(dftotals$opp),2)] <- dftotals$teams[seq(0,length(dftotals$teams2),2)]
    dftotals$opp <- as.character(dftotals$opp)
    
    dftotals <- transform(dftotals, opener = colsplit(opener2, pattern = "\\s", names=c("odds","juice"))) 
    dftotals <- transform(dftotals, pinn = colsplit(pinn2, pattern = "\\s", names=c("odds","totsjuice"))) 
    
    
    dftotals$pinn.totals <- as.numeric(gsub('\\PK-110','0', dftotals$pinn.odds))
    dftotals$opener.totals <- as.numeric(gsub('\\PK-110','0', dftotals$opener.odds ))
    
    
    dftotals$opener.juice[is.na(dftotals$opener.juice)] <- as.numeric("-110")
    dftotals$pinn.totsjuice[is.na(dftotals$pinn.totsjuice)] <- as.numeric("-110")
    dftotals <- dftotals[,c(1,8,7,6)]
    names(dftotals) <- c("teams", "opener", "totals", "totalsjuice")
  }
  if(nrow(dfspreads)>0){
    oddsframe <- left_join(dfspreads, dftotals, c("teams" = "teams"))
    return(oddsframe)
  }
  else{
    return(-1)
  }
  
  
}

get_Odds_Between <- function(startdate, enddate) {
  gamedates <- seq.Date(as.Date(startdate, "%m/%d/%Y"), as.Date(enddate, "%m/%d/%Y"), "day")
  all_data <- lapply(gamedates, get_DateOdds)
  non_empty_day <- all_data %>% map_lgl(is.data.frame)
  all_data <- all_data[non_empty_day]
  all_data <- bind_rows(all_data)
  return(all_data)
}