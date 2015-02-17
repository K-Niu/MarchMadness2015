regularseason <- read.csv("regular_season_detailed_results.csv")
regularseason_compact <- read.csv("regular_season_compact_results.csv")
tourney <- read.csv("tourney_detailed_results.csv")
tourney_compact <- read.csv("tourney_compact_results.csv")
tourneyseeds <- read.csv("tourney_seeds.csv")
tourneyslots <- read.csv("tourney_slots.csv")
seasons <- read.csv("seasons.csv")
teams <- read.csv("teams.csv")
sample <- read.csv("sample_submission.csv")

#Create training set from years 2003 - 2010
train <- data.frame(team1 = integer(0), team2 = integer(0), fgPer1 = numeric(0), fg3Per1 = numeric(0), ftPer1 = numeric(0), wlRat1 = numeric(0), ptRat1 = numeric(0), avgor1 = numeric(0), avgdr1 = numeric(0), avgast1 = numeric(0), avgto1 = numeric(0), avgstl1 = numeric(0), avgblk1 = numeric(0), avgpf1 = numeric(0), seed1 = integer(0),
                                                            fgPer2 = numeric(0), fg3Per2 = numeric(0), ftPer2 = numeric(0), wlRat2 = numeric(0), ptRat2 = numeric(0), avgor2 = numeric(0), avgdr2 = numeric(0), avgast2 = numeric(0), avgto2 = numeric(0), avgstl2 = numeric(0), avgblk2 = numeric(0), avgpf2 = numeric(0), seed2 = integer(0),
                    won = integer(0))

for(i in 2003:2010) {
  statsi <- data.frame(team_id = teams$team_id) #Stores current regular season information for each team
  statsi$totalfgm <- 0 #Field goals made
  statsi$totalfga <- 0 #Field goals attempted
  statsi$totalfgm3 <- 0 #3 pointers made
  statsi$totalfga3 <- 0 #3 pointers attempted
  statsi$totalftm <- 0 #Free throws made
  statsi$totalfta <- 0 #Free throws attempted
  statsi$totalor <- 0 #Offensive rebounds
  statsi$totaldr <- 0 #Deffensive rebounds
  statsi$totalast <- 0 #Assists
  statsi$totalto <- 0 #Turnovers
  statsi$totalstl <- 0 #Steals
  statsi$totalblk <- 0 #Blocks
  statsi$totalpf <- 0 #Personal fouls
  statsi$totalwins <- 0 #Wins
  statsi$totallosses <- 0 #Losses
  statsi$totalpt <- 0 #Points scored
  statsi$totalptagainst <- 0 #Points scored against
  
  regularseasoni <- regularseason[regularseason$season == i,]
  for(j in 1:length(regularseasoni$season)) {
    winner <- regularseasoni$wteam[j]
    loser <- regularseasoni$lteam[j]
    
    statsi$totalfgm[winner - 1100] = statsi$totalfgm[winner - 1100] + regularseasoni$wfgm[j]
    statsi$totalfga[winner - 1100] = statsi$totalfga[winner - 1100] + regularseasoni$wfga[j]
    statsi$totalfgm3[winner - 1100] = statsi$totalfgm3[winner - 1100] + regularseasoni$wfgm3[j]
    statsi$totalfga3[winner - 1100] = statsi$totalfga3[winner - 1100] + regularseasoni$wfga3[j]
    statsi$totalftm[winner - 1100] = statsi$totalftm[winner - 1100] + regularseasoni$wftm[j]
    statsi$totalfta[winner - 1100] = statsi$totalfta[winner - 1100] + regularseasoni$wfta[j]
    statsi$totalor[winner - 1100] = statsi$totalor[winner - 1100] + regularseasoni$wor[j]
    statsi$totaldr[winner - 1100] = statsi$totaldr[winner - 1100] + regularseasoni$wdr[j]
    statsi$totalast[winner - 1100] = statsi$totalast[winner - 1100] + regularseasoni$wast[j]
    statsi$totalto[winner - 1100] = statsi$totalto[winner - 1100] + regularseasoni$wto[j]
    statsi$totalstl[winner - 1100] = statsi$totalstl[winner - 1100] + regularseasoni$wstl[j]
    statsi$totalblk[winner - 1100] = statsi$totalblk[winner - 1100] + regularseasoni$wblk[j]
    statsi$totalpf[winner - 1100] = statsi$totalpf[winner - 1100] + regularseasoni$wpf[j]
    statsi$totalwins[winner - 1100] = statsi$totalwins[winner - 1100] + 1
    statsi$totalpt[winner - 1100] = statsi$totalpt[winner - 1100] + regularseasoni$wscore[j]
    statsi$totalptagainst[winner - 1100] = statsi$totalptagainst[winner - 1100] + regularseasoni$lscore[j]
    
    statsi$totalfgm[loser - 1100] = statsi$totalfgm[loser - 1100] + regularseasoni$lfgm[j]
    statsi$totalfga[loser - 1100] = statsi$totalfga[loser - 1100] + regularseasoni$lfga[j]
    statsi$totalfgm3[loser - 1100] = statsi$totalfgm3[loser - 1100] + regularseasoni$lfgm3[j]
    statsi$totalfga3[loser - 1100] = statsi$totalfga3[loser - 1100] + regularseasoni$lfga3[j]
    statsi$totalftm[loser - 1100] = statsi$totalftm[loser - 1100] + regularseasoni$lftm[j]
    statsi$totalfta[loser - 1100] = statsi$totalfta[loser - 1100] + regularseasoni$lfta[j]
    statsi$totalor[loser - 1100] = statsi$totalor[loser - 1100] + regularseasoni$lor[j]
    statsi$totaldr[loser - 1100] = statsi$totaldr[loser - 1100] + regularseasoni$ldr[j]
    statsi$totalast[loser - 1100] = statsi$totalast[loser - 1100] + regularseasoni$last[j]
    statsi$totalto[loser - 1100] = statsi$totalto[loser - 1100] + regularseasoni$lto[j]
    statsi$totalstl[loser - 1100] = statsi$totalstl[loser - 1100] + regularseasoni$lstl[j]
    statsi$totalblk[loser - 1100] = statsi$totalblk[loser - 1100] + regularseasoni$lblk[j]
    statsi$totalpf[loser - 1100] = statsi$totalpf[loser - 1100] + regularseasoni$lpf[j]
    statsi$totallosses[loser - 1100] = statsi$totallosses[loser - 1100] + 1
    statsi$totalpt[loser - 1100] = statsi$totalpt[loser - 1100] + regularseasoni$lscore[j]
    statsi$totalptagainst[loser - 1100] = statsi$totalptagainst[loser - 1100] + regularseasoni$wscore[j]
  }
  statsi$fgPer <- statsi$totalfgm/statsi$totalfga #Field goal percentage
  statsi$fg3Per <- statsi$totalfgm3/statsi$totalfga3 #3 point percentage
  statsi$ftPer <- statsi$totalftm/statsi$totalfta #Free throw percentage
  statsi$wlRat <- statsi$totalwins/statsi$totallosses #Win-loss ratio
  statsi$ptRat <- statsi$totalpt/statsi$totalptagainst #Points scored to points scored against ratio
  statsi$avgor <- statsi$totalor/(statsi$totalwins + statsi$totallosses) #Average offensive rebounds
  statsi$avgdr <- statsi$totaldr/(statsi$totalwins + statsi$totallosses) #Average defensive rebounds
  statsi$avgast <- statsi$totalast/(statsi$totalwins + statsi$totallosses) #Average assists
  statsi$avgto <- statsi$totalto/(statsi$totalwins + statsi$totallosses) #Average turn overs
  statsi$avgstl <- statsi$totalstl/(statsi$totalwins + statsi$totallosses) #Average steals
  statsi$avgblk <- statsi$totalblk/(statsi$totalwins + statsi$totallosses) #Average blocks
  statsi$avgpf <- statsi$totalpf/(statsi$totalwins + statsi$totallosses) #Average personal fouls
  
  tourneyi <- tourney[tourney$season == i,]
  tourneyseedsi <- tourneyseeds[tourneyseeds$season == i,]
  #GET SEEDS
  firstteam <- tourneyi$wteam
  secondteam <- tourneyi$lteam
  fgPer1 <- vector()
  fg3Per1 <- vector()
  ftPer1 <- vector()
  wlRat1 <- vector()
  ptRat1 <- vector()
  avgor1 <- vector()
  avgdr1 <- vector()
  avgast1 <- vector()
  avgto1 <- vector()
  avgstl1 <- vector()
  avgblk1 <- vector()
  avgpf1 <- vector()
  fgPer2 <- vector()
  fg3Per2 <- vector()
  ftPer2 <- vector()
  wlRat2 <- vector()
  ptRat2 <- vector()
  avgor2 <- vector()
  avgdr2 <- vector()
  avgast2 <- vector()
  avgto2 <- vector()
  avgstl2 <- vector()
  avgblk2 <- vector()
  avgpf2 <- vector()
  seed1 <- vector()
  seed2 <- vector()
  for(j in 1:length(tourneyi$wteam)) {
    fgPer1 = rbind(fgPer1, statsi$fgPer[tourneyi$wteam[j] - 1100])
    fg3Per1 = rbind(fg3Per1, statsi$fg3Per[tourneyi$wteam[j] - 1100])
    ftPer1 = rbind(ftPer1, statsi$ftPer[tourneyi$wteam[j] - 1100])
    wlRat1 = rbind(wlRat1, statsi$wlRat[tourneyi$wteam[j] - 1100])
    ptRat1 = rbind(ptRat1, statsi$ptRat[tourneyi$wteam[j] - 1100])
    avgor1 = rbind(avgor1, statsi$avgor[tourneyi$wteam[j] - 1100])
    avgdr1 = rbind(avgdr1, statsi$avgdr[tourneyi$wteam[j] - 1100])
    avgast1 = rbind(avgast1, statsi$avgast[tourneyi$wteam[j] - 1100])
    avgto1 = rbind(avgto1, statsi$avgto[tourneyi$wteam[j] - 1100])
    avgstl1 = rbind(avgstl1, statsi$avgstl[tourneyi$wteam[j] - 1100])
    avgblk1 = rbind(avgblk1, statsi$avgblk[tourneyi$wteam[j] - 1100])
    avgpf1 = rbind(avgpf1, statsi$avgpf[tourneyi$wteam[j] - 1100])
    fgPer2 = rbind(fgPer2, statsi$fgPer[tourneyi$lteam[j] - 1100])
    fg3Per2 = rbind(fg3Per2, statsi$fg3Per[tourneyi$lteam[j] - 1100])
    ftPer2 = rbind(ftPer2, statsi$ftPer[tourneyi$lteam[j] - 1100])
    wlRat2 = rbind(wlRat2, statsi$wlRat[tourneyi$lteam[j] - 1100])
    ptRat2 = rbind(ptRat2, statsi$ptRat[tourneyi$lteam[j] - 1100])
    avgor2 = rbind(avgor2, statsi$avgor[tourneyi$lteam[j] - 1100])
    avgdr2 = rbind(avgdr2, statsi$avgdr[tourneyi$lteam[j] - 1100])
    avgast2 = rbind(avgast2, statsi$avgast[tourneyi$lteam[j] - 1100])
    avgto2 = rbind(avgto2, statsi$avgto[tourneyi$lteam[j] - 1100])
    avgstl2 = rbind(avgstl2, statsi$avgstl[tourneyi$lteam[j] - 1100])
    avgblk2 = rbind(avgblk2, statsi$avgblk[tourneyi$lteam[j] - 1100])
    avgpf2 = rbind(avgpf2, statsi$avgpf[tourneyi$lteam[j] - 1100])
    seed1 = rbind(seed1, ) #GET SEEDS
    seed2 = rbind(seed2, ) #GET SEEDS
  }
}