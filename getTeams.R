library(stattleshipR)

set_token("18efec0cec8943fa9f5397516e4a6809")

tl <- read.csv("tl.csv")

sport <- 'football'
league <- 'nfl'
ep <- 'teams'

q_body <- list()
teams <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, version=1, verbose=TRUE, walk=TRUE)

the_teams <- do.call('rbind', lapply(teams, function(x) x$teams))
the_teams$abbrev <- tl[match(the_teams$nickname, tl$teams),]$abbrev

save(the_teams, file="/home/ec2-user/DFS/NFL/the_teams.Rdata")

