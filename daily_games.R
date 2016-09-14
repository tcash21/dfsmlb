library(stattleshipR)

set_token("18efec0cec8943fa9f5397516e4a6809")

setwd("/home/ec2-user/DFS/NFL")
#load("team_lookup.Rdata")

sport <- 'football'
league <- 'nfl'
ep<-'games'
q_body <- list(status='upcoming', week=2)
upcoming_games <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, version=1, verbose=TRUE, walk=TRUE)
ug<-do.call('rbind', lapply(upcoming_games, function(x) x$games))
ug <- ug[match(unique(ug$title), ug$title),]

save(ug, file="/home/ec2-user/DFS/NFL/tonights_games_nfl.Rdata")
