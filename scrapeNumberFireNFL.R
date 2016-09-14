library(rvest)
library(stringr)

scrapeNumberFire <- function(URL){
  nf <- URL
  nf_preds <- nf %>%
    html() %>%
    html_nodes(xpath='//*[@class="projection-table__body"]')
  parsed.xml <- xmlTreeParse(nf_preds[[1]])
  parsed.xml2 <- xmlTreeParse(nf_preds[[2]])
  
  players <- parsed.xml$doc$children$tbody
  stats <- parsed.xml2$doc$children$tbody
  
  players_list <- getNodeSet(parsed.xml$doc$children$tbody, "//span[@class='full']")
  player_names <- unlist(lapply(players_list, function (x) xmlValue(x)))
  positions <- getNodeSet(players, '//td')
  player_pos <- unlist(lapply(positions, function(x) xmlValue(x)))
  stats_list <- getNodeSet(parsed.xml2$doc$children$tbody, "//tr")
  stat_values_list <- sapply(stats_list, function (x) getNodeSet(x, "//td"))
  
  if(type == 'Fanduel'){
    if(URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k'){
      salaries <- apply(stat_values_list, 2, function(x) x[15])
      projections <- apply(stat_values_list, 2, function(x) x[14])
    } else if (URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections'){
      projections <- apply(stat_values_list, 2, function(x) x[15])
      salaries <- apply(stat_values_list, 2, function(x) x[16])
    } else if (URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d'){
      projections <- apply(stat_values_list, 2, function(x) x[12])
      salaries <- apply(stat_values_list, 2, function(x) x[13])
    }
    
  } else if (type == 'yahoo'){
    if(URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k'){
      projections <- apply(stat_values_list, 2, function(x) x[38])
      salaries <- apply(stat_values_list, 2, function(x) x[40])
    } else if (URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections'){
      projections <- apply(stat_values_list, 2, function(x) x[39])
      salaries <- apply(stat_values_list, 2, function(x) x[40])
    } else if (URL == 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d'){
      projections <- apply(stat_values_list, 2, function(x) x[36])
      salaries <- apply(stat_values_list, 2, function(x) x[37])
    }
  }
  
  projections <- as.numeric(unlist(lapply(projections, function(x) xmlValue(x[[1]]))))
  salaries <- unlist(lapply(salaries, function(x) xmlValue(x[[1]])))
  
  nf_pred_df <- data.frame(player=player_names, position=player_pos, nf_pred=projections, salary = salaries)
  
  #  name_only <- unlist(lapply(str_match_all(nf_pred_df$player, '(\\w.*)\\s\\('), function(x) x[2]))
  pos_only <- unlist(lapply(str_match_all(nf_pred_df$position, '\\((\\w+),') , function(x) x[2]))
  team_only <- unlist(lapply(str_match_all(nf_pred_df$position, '(\\w+)\\)') , function(x) x[2]))
  nf_pred_df <- data.frame(player=player_names, nf_pred=projections, team=team_only, Salary=salaries, Position=pos_only)
  nf_pred_df$Salary <- as.numeric(gsub("\\$|,", "", nf_pred_df$Salary))
  #  nf_pred_df$name <- name_only
  
  nf_pred_df$team<-as.factor(nf_pred_df$team)
  nf_pred_df$Position<-as.factor(nf_pred_df$Position)
  nf_pred_df$player <- as.factor(nf_pred_df$player)
  
  return(nf_pred_df)
}
