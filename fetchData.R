library(lpSolve)
library(rvest)
library(XML)
library(RCurl)
library(reshape2)

setwd("/home/ec2-user/DFS/NFL")

source("scrapeNumberFireNFL.R")

#teamLookup <- read.csv("tl.csv")
tl<-read.csv("tl.csv", header=TRUE)
load("tonights_games_nfl.Rdata")


tl$abbrev<-as.character(tl$abbrev)
#tl$fanduel <-teamLookup[match(tl$teams, teamLookup$NFL_team),]$fan

URL <- 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections'
URL2 <- 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k'
URL3 <- 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d'

type <- 'Fanduel'

offense <- scrapeNumberFire(URL)
kickers <- scrapeNumberFire(URL2)
defense <- scrapeNumberFire(URL3)

new.week<-rbind(offense, kickers, defense)
new.week <- subset(new.week, nf_pred != 0 & !is.na(Position) )
new.week <- subset(new.week, !is.na(Salary))
new.week$team <- as.character(new.week$team)
new.week$Position <- as.character(new.week$Position)
new.week$player <- as.character(new.week$player)
new.week$nickname <- tl[match(new.week$team, tl$abbrev),]$teams

library(rvest)

url <- 'https://www.sportsbook.ag/sbk/sportsbook4/nfl-betting/nfl-lines.sbk'
game.lines <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="_"]/div[1]/div[2]/div[1]') 

game.lines <- lapply(game.lines, xmlParse)
the.lines <-lapply(game.lines, function(x) xmlToDataFrame(x)[2,])
the.lines <- as.numeric(unlist(lapply(str_match_all(the.lines, "(\\d+\\.?\\d?)\\("), function(x) x[2])))

away.teams <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="_"]/div[1]/div[1]/div/div')

away.teams <- lapply(away.teams, xmlParse)
away.teams <- lapply(lapply(away.teams, function(x) getNodeSet(x, "//span")), function(x) x[1])
away.teams <- unlist(lapply(unlist(away.teams), xmlValue))

home.teams <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="_"]/div[2]/div[1]/div/div')

home.teams <- lapply(home.teams, xmlParse)
home.teams <- lapply(lapply(home.teams, function(x) getNodeSet(x, "//span")), function(x) x[1])
home.teams <- unlist(lapply(unlist(home.teams), xmlValue))

the.spreads <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="_"]/div[1]/div[2]/div[2]')

the.spreads <- lapply(the.spreads, xmlParse)
the.spreads <-lapply(the.spreads, function(x) xmlToDataFrame(x)[2,])
the.spreads <- unlist(lapply(str_match_all(the.spreads, "([+-]\\d+\\.?\\d?)\\("), function(x) x[2]))
the.spreads<-as.numeric(gsub("\\+", "", the.spreads))

vegas<-data.frame(away.team=away.teams, home.team=home.teams, lines=the.lines, spread=the.spreads)
vegas$fan.away <- tl[match(vegas$away.team, tl$yahoo),]$abbrev
vegas$fan.home <- tl[match(vegas$home.team, tl$yahoo),]$abbrev
v<-melt(vegas[,3:6], measure.vars = c("lines", "spread"))

v2<-data.frame(team=c(as.character(v[which(v$variable == "spread"),]$fan.home), as.character(v[which(v$variable == "spread"),]$fan.away)),
               spread=c(v[which(v$variable == "spread"),]$value, v[which(v$variable == "spread"),]$value),
               line=c(v[which(v$variable == "lines"),]$value, v[which(v$variable == "lines"),]$value))
v2[which(v2$team %in% vegas$fan.away ),]$spread <-  v2[which(v2$team %in% vegas$fan.away ),]$spread * -1

points_against <- list()
positions <- c("QB", "WR", "RB", "TE", "K" ,'DEF')
for (pos in positions){
  pa_url <- paste0("https://football.fantasysports.yahoo.com/f1/pointsagainst?season=2016&pos=", pos, "&mode=average")
  html <- getURL(pa_url, .opts = list(ssl.verifypeer=FALSE))
  doc = htmlParse(html, asText=TRUE)
  table_data <- xpathSApply(doc, "//*[@id='statTable0']/tbody")
  points_against[[pos]] <- xmlToDataFrame(table_data[[1]])
  yahoo.teams <- as.character(points_against[[pos]][,2])
  yahoo.teams <- gsub("\\s+vs.*", "", yahoo.teams)
  points_against[[pos]]$Team <- yahoo.teams
}

the_teams<-lapply(points_against, function(x) x$Team)
pa<-lapply(points_against, function(x) x[,which(colnames(x) == 'Team') - 1])

results <- list()

for(i in 1:length(the_teams)){
  results[[i]] <- data.frame(the_teams[[i]], pa[[i]], names(the_teams)[i])
}

pa<-do.call('rbind', results)
colnames(pa) <- c("team", "pa", "position")

home_teams <- unlist(lapply(str_match_all(ug$title, "vs\\s+(\\w+)"), function(x) x[2]))
away_teams <- unlist(lapply(str_match_all(ug$title, "(\\w+)\\s+vs"), function(x) x[2]))
the_games <- data.frame(home=home_teams, away=away_teams)

new.week$Opponent <- ''
new.week$Opponent <- the_games[match(new.week$nickname, the_games$home),]$away
x <- the_games[match(new.week$nickname, the_games$away),]$home
x <- as.character(x[-which(is.na(x))])
new.week$Opponent<-as.character(new.week$Opponent)
new.week[which(is.na(new.week$Opponent)),]$Opponent <- x

pa$abbrev<-tl[match(pa$team, tl$yahoo),]$abb
colnames(pa)[4] <- 'Opponent'
pa$position <- as.character(pa$position)
pa$position[pa$position == 'DEF'] <- 'D'
pa$key <- paste0(pa$Opponent, pa$position)
new.week$OpponentAbbrev<-tl[match(new.week$Opponent, tl$teams),]$abbrev
new.week$key <- paste0(new.week$OpponentAbbrev, new.week$Position)
all<-merge(pa, new.week, by='key')

v2$abbrev<-tl[match(v2$team, tl$abbrev),]$abbrev
v2 <- v2[,-1]
colnames(v2)[3] <- 'Team'

all <- all[,-1:-2]
colnames(all)[6] <- 'Team'

all<-merge(all, v2, by='Team')
all$Position <- as.character(all$Position)
all$Team <- as.character(all$Team)
all$pa<-as.numeric(as.character(all$pa))


all$score <- (all$nf_pred * 0.6) + (all$pa * 0.3) + ((all$spread * - 1) * 0.10)

#obj <- all$score
#con <- rbind(t(model.matrix(~ Position + 0,all)), t(model.matrix(~ Team + 0, all)), rep(1,nrow(all)), all$Salary)
#dir <- c("=","=","=","=","=", "=", rep('<=',length(unique(all$Team))),"=","<=")
#rhs <- c(1,1,1,2,1,3,rep(4,length(unique(all$team))),9,60000)
#result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)
#results <- all[which(result$solution == 1),]
#results

all$value <- all$Salary / all$nf_pred
all$title <- ug[apply(all, 1, function(x) grep(x['nickname'], ug$title)),]$title

if(length(which(is.na(all$line))) > 0){
        all <- all[-which(is.na(all$line)),]
}


write.csv(all, file="preds_nfl.csv", row.names=FALSE)

system("sh /home/ec2-user/DFS/NFL/export_to_s3.sh")
system("touch /srv/shiny-server/NFL/restart.txt")

