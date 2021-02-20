current_eff <- read_html("http://stats.inpredictable.com/nba/ssnTeamPoss.php?season=2020&po=0&frdt=2021-02-08&todt=2021-02-19&view=off&sort=aotop&order=ASC") %>% html_table(fill = TRUE)
current_eff <- current_eff[[1]]
names(current_eff) <- current_eff[2,]
current_eff <- current_eff[-c(1:2, 33,34),]
names(current_eff)[5] <- "OPL"
current_eff$OPL <- as.numeric(current_eff$OPL)
write.csv(current_eff, "current_eff.csv")