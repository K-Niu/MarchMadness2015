regularseason <- read.csv("regular_season_detailed_results.csv")
regularseason_compact <- read.csv("regular_season_compact_results.csv")
tourney <- read.csv("tourney_detailed_results.csv")
tourney_compact <- read.csv("tourney_compact_results.csv")
tourneyseeds <- read.csv("tourney_seeds.csv")
tourneyslots <- read.csv("tourney_slots.csv")
seasons <- read.csv("seasons.csv")
teams <- read.csv("teams.csv")
sample <- read.csv("sample_submission.csv")

#Trying to use regular season stats to predict tourney performance
stats <- data.frame(team_id = teams$team_id)
stats$totalfgm <- 0 #Field goals made
stats$totalfga <- 0 #Field goals attempted
stats$totalfgm3 <- 0 #3 pointers made
stats$totalfga3 <- 0 #3 pointers attempted
stats$totalftm <- 0 #Free throws made
stats$totalfta <- 0 #Free throws attempted
stats$totalor <- 0 #Offensive rebounds
stats$totaldr <- 0 #Deffensive rebounds
stats$totalast <- 0 #Assists
stats$totalto <- 0 #Turnovers
stats$totalstl <- 0 #Steals
stats$totalblk <- 0 #Blocks
stats$totalpf <- 0 #Personal fouls
stats$totalwins <- 0 #Wins
stats$totallosses <- 0 #Losses
stats$totalpt <- 0 #Points scored
stats$totalptagainst <- 0 #Points scored against

for(i in 1:length(regularseason$season)) {
  winner <- regularseason$wteam[i]
  loser <- regularseason$lteam[i]
  
  stats$totalfgm[winner - 1100] = stats$totalfgm[winner - 1100] + regularseason$wfgm[i]
  stats$totalfga[winner - 1100] = stats$totalfga[winner - 1100] + regularseason$wfga[i]
  stats$totalfgm3[winner - 1100] = stats$totalfgm3[winner - 1100] + regularseason$wfgm3[i]
  stats$totalfga3[winner - 1100] = stats$totalfga3[winner - 1100] + regularseason$wfga3[i]
  stats$totalftm[winner - 1100] = stats$totalftm[winner - 1100] + regularseason$wftm[i]
  stats$totalfta[winner - 1100] = stats$totalfta[winner - 1100] + regularseason$wfta[i]
  stats$totalor[winner - 1100] = stats$totalor[winner - 1100] + regularseason$wor[i]
  stats$totaldr[winner - 1100] = stats$totaldr[winner - 1100] + regularseason$wdr[i]
  stats$totalast[winner - 1100] = stats$totalast[winner - 1100] + regularseason$wast[i]
  stats$totalto[winner - 1100] = stats$totalto[winner - 1100] + regularseason$wto[i]
  stats$totalstl[winner - 1100] = stats$totalstl[winner - 1100] + regularseason$wstl[i]
  stats$totalblk[winner - 1100] = stats$totalblk[winner - 1100] + regularseason$wblk[i]
  stats$totalpf[winner - 1100] = stats$totalpf[winner - 1100] + regularseason$wpf[i]
  stats$totalwins[winner - 1100] = stats$totalwins[winner - 1100] + 1
  stats$totalpt[winner - 1100] = stats$totalpt[winner - 1100] + regularseason$wscore[i]
  stats$totalptagainst[winner - 1100] = stats$totalptagainst[winner - 1100] + regularseason$lscore[i]
  
  stats$totalfgm[loser - 1100] = stats$totalfgm[loser - 1100] + regularseason$lfgm[i]
  stats$totalfga[loser - 1100] = stats$totalfga[loser - 1100] + regularseason$lfga[i]
  stats$totalfgm3[loser - 1100] = stats$totalfgm3[loser - 1100] + regularseason$lfgm3[i]
  stats$totalfga3[loser - 1100] = stats$totalfga3[loser - 1100] + regularseason$lfga3[i]
  stats$totalftm[loser - 1100] = stats$totalftm[loser - 1100] + regularseason$lftm[i]
  stats$totalfta[loser - 1100] = stats$totalfta[loser - 1100] + regularseason$lfta[i]
  stats$totalor[loser - 1100] = stats$totalor[loser - 1100] + regularseason$lor[i]
  stats$totaldr[loser - 1100] = stats$totaldr[loser - 1100] + regularseason$ldr[i]
  stats$totalast[loser - 1100] = stats$totalast[loser - 1100] + regularseason$last[i]
  stats$totalto[loser - 1100] = stats$totalto[loser - 1100] + regularseason$lto[i]
  stats$totalstl[loser - 1100] = stats$totalstl[loser - 1100] + regularseason$lstl[i]
  stats$totalblk[loser - 1100] = stats$totalblk[loser - 1100] + regularseason$lblk[i]
  stats$totalpf[loser - 1100] = stats$totalpf[loser - 1100] + regularseason$lpf[i]
  stats$totallosses[loser - 1100] = stats$totallosses[loser - 1100] + 1
  stats$totalpt[loser - 1100] = stats$totalpt[loser - 1100] + regularseason$lscore[i]
  stats$totalptagainst[loser - 1100] = stats$totalptagainst[loser - 1100] + regularseason$wscore[i]
}

stats$fgPer <- stats$totalfgm/stats$totalfga #Field goal percentage
stats$fg3Per <- stats$totalfgm3/stats$totalfga3 #3 point percentage
stats$ftPer <- stats$totalftm/stats$totalfta #Free throw percentage
stats$wlRat <- stats$totalwins/stats$totallosses #Win-loss ratio
stats$ptRat <- stats$totalpt/stats$totalptagainst #Points scored to points scored against ratio

write.csv(stats, "stats.csv", row.names = FALSE)
stats <- read.csv("stats.csv")

#Create train set
train1 <- data.frame(team1 = regularseason$wteam, team2 = regularseason$lteam)
train1$fgPer1 <- 0
train1$fg3Per1 <- 0
train1$ftPer1 <- 0
train1$wlRat1 <- 0
train1$ptRat1 <- 0
train1$avgor1 <- 0
train1$avgdr1 <- 0
train1$avgast1 <- 0
train1$avgto1 <- 0
train1$avgstl1 <- 0
train1$avgblk1 <- 0
train1$avgpf1 <- 0

train1$fgPer2 <- 0
train1$fg3Per2 <- 0
train1$ftPer2 <- 0
train1$wlRat2 <- 0
train1$ptRat2 <- 0
train1$avgor2 <- 0
train1$avgdr2 <- 0
train1$avgast2 <- 0
train1$avgto2 <- 0
train1$avgstl2 <- 0
train1$avgblk2 <- 0
train1$avgpf2 <- 0

train1$won <- 1

for(i in 1:length(train1$team1)) {
  train1$fgPer1[i] = stats$fgPer[train1$team1[i] - 1100]
  train1$fg3Per1[i] = stats$fg3Per[train1$team1[i] - 1100]
  train1$ftPer1[i] = stats$ftPer[train1$team1[i] - 1100]
  train1$wlRat1[i] = stats$wlRat[train1$team1[i] - 1100]
  train1$ptRat1[i] = stats$ptRat[train1$team1[i] - 1100]
  train1$avgor1[i] = stats$totalor[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgdr1[i] = stats$totaldr[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgast1[i] = stats$totalast[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgto1[i] = stats$totalto[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgstl1[i] = stats$totalstl[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgblk1[i] = stats$totalblk[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  train1$avgpf1[i] = stats$totalpf[train1$team1[i] - 1100]/(stats$totalwins[train1$team1[i] - 1100] + stats$totallosses[train1$team1[i] - 1100])
  
  train1$fgPer2[i] = stats$fgPer[train1$team2[i] - 1100]
  train1$fg3Per2[i] = stats$fg3Per[train1$team2[i] - 1100]
  train1$ftPer2[i] = stats$ftPer[train1$team2[i] - 1100]
  train1$wlRat2[i] = stats$wlRat[train1$team2[i] - 1100]
  train1$ptRat2[i] = stats$ptRat[train1$team2[i] - 1100]
  train1$avgor2[i] = stats$totalor[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgdr2[i] = stats$totaldr[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgast2[i] = stats$totalast[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgto2[i] = stats$totalto[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgstl2[i] = stats$totalstl[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgblk2[i] = stats$totalblk[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
  train1$avgpf2[i] = stats$totalpf[train1$team2[i] - 1100]/(stats$totalwins[train1$team2[i] - 1100] + stats$totallosses[train1$team2[i] - 1100])
}

train2 <- data.frame(team1 = train1$team2, team2 = train1$team1, 
                    fgPer1 = train1$fgPer2,
                    fg3Per1 = train1$fg3Per2,
                    ftPer1 = train1$ftPer2,
                    wlRat1 = train1$wlRat2,
                    ptRat1 = train1$ptRat2,
                    avgor1 = train1$avgor2,
                    avgdr1 = train1$avgdr2,
                    avgast1 = train1$avgast2,
                    avgto1 = train1$avgto2,
                    avgstl1 = train1$avgstl2,
                    avgblk1 = train1$avgblk2,
                    avgpf1 = train1$avgpf2,
                    fgPer2 = train1$fgPer1,
                    fg3Per2 = train1$fg3Per1,
                    ftPer2 = train1$ftPer1,
                    wlRat2 = train1$wlRat1,
                    ptRat2 = train1$ptRat1,
                    avgor2 = train1$avgor1,
                    avgdr2 = train1$avgdr1,
                    avgast2 = train1$avgast1,
                    avgto2 = train1$avgto1,
                    avgstl2 = train1$avgstl1,
                    avgblk2 = train1$avgblk1,
                    avgpf2 = train1$avgpf1,
                    won = 0)

train <- rbind(train1, train2)

write.csv(train, "train.csv", row.names = FALSE)
train <- read.csv("train.csv")

#Create test set
tourney2011 <- tourney[tourney$season == 2011,]
tourney2012 <- tourney[tourney$season == 2012,]
tourney2013 <- tourney[tourney$season == 2013,]
tourney2014 <- tourney[tourney$season == 2014,]
teams2011 <- vector()
teams2012 <- vector()
teams2013 <- vector()
teams2014 <- vector()

for(i in 1:length(tourney2011$season)) {
  if(!(tourney2011$wteam[i] %in% teams2011)) {
    teams2011 = rbind(teams2011, tourney2011$wteam[i])
  }
  if(!(tourney2011$lteam[i] %in% teams2011)) {
    teams2011 = rbind(teams2011, tourney2011$lteam[i])
  }
}
for(i in 1:length(tourney2012$season)) {
  if(!(tourney2012$wteam[i] %in% teams2012)) {
    teams2012 = rbind(teams2012, tourney2012$wteam[i])
  }
  if(!(tourney2012$lteam[i] %in% teams2012)) {
    teams2012 = rbind(teams2012, tourney2012$lteam[i])
  }
}
for(i in 1:length(tourney2013$season)) {
  if(!(tourney2013$wteam[i] %in% teams2013)) {
    teams2013 = rbind(teams2013, tourney2013$wteam[i])
  }
  if(!(tourney2013$lteam[i] %in% teams2013)) {
    teams2013 = rbind(teams2013, tourney2013$lteam[i])
  }
}
for(i in 1:length(tourney2014$season)) {
  if(!(tourney2014$wteam[i] %in% teams2014)) {
    teams2014 = rbind(teams2014, tourney2014$wteam[i])
  }
  if(!(tourney2014$lteam[i] %in% teams2014)) {
    teams2014 = rbind(teams2014, tourney2014$lteam[i])
  }
}

teams2011 = sort(teams2011)
teams2012 = sort(teams2012)
teams2013 = sort(teams2013)
teams2014 = sort(teams2014)

firstteam <- vector()
secondteam <- vector()
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
for(i in 1:length(teams2011)) {
  for(j in (i + 1):length(teams2011)) {
    if(j <= length(teams2011) & i != j) {
      firstteam = rbind(firstteam, teams2011[i])
      secondteam = rbind(secondteam, teams2011[j])
      fgPer1 = rbind(fgPer1, stats$fgPer[teams2011[i] - 1100])
      fg3Per1 = rbind(fg3Per1, stats$fg3Per[teams2011[i] - 1100])
      ftPer1 = rbind(ftPer1, stats$ftPer[teams2011[i] - 1100])
      wlRat1 = rbind(wlRat1, stats$wlRat[teams2011[i] - 1100])
      ptRat1 = rbind(ptRat1, stats$ptRat[teams2011[i] - 1100])
      avgor1 = rbind(avgor1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgdr1 = rbind(avgdr1, stats$totaldr[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgast1 = rbind(avgast1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgto1 = rbind(avgto1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgstl1 = rbind(avgstl1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgblk1 = rbind(avgblk1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      avgpf1 = rbind(avgpf1, stats$totalor[teams2011[i] - 1100]/(stats$totalwins[teams2011[i] - 1100] + stats$totallosses[teams2011[i] - 1100]))
      fgPer2 = rbind(fgPer2, stats$fgPer[teams2011[j] - 1100])
      fg3Per2 = rbind(fg3Per2, stats$fg3Per[teams2011[j] - 1100])
      ftPer2 = rbind(ftPer2, stats$ftPer[teams2011[j] - 1100])
      wlRat2 = rbind(wlRat2, stats$wlRat[teams2011[j] - 1100])
      ptRat2 = rbind(ptRat2, stats$ptRat[teams2011[j] - 1100])
      avgor2 = rbind(avgor2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgdr2 = rbind(avgdr2, stats$totaldr[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgast2 = rbind(avgast2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgto2 = rbind(avgto2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgstl2 = rbind(avgstl2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgblk2 = rbind(avgblk2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
      avgpf2 = rbind(avgpf2, stats$totalor[teams2011[j] - 1100]/(stats$totalwins[teams2011[j] - 1100] + stats$totallosses[teams2011[j] - 1100]))
    }
  }
}
matchups2011 <- data.frame(team1 = firstteam, team2 = secondteam, fgPer1 = fgPer1, fg3Per1 = fg3Per1, ftPer1 = ftPer1, wlRat1 = wlRat1, ptRat1 = ptRat1, avgor1 = avgor1, avgdr1 = avgdr1, avgast1 = avgast1, avgto1 = avgto1, avgstl1 = avgstl1, avgblk1 = avgblk1, avgpf1 = avgpf1, fgPer2 = fgPer2, fg3Per2 = fg3Per2, ftPer2 = ftPer2, wlRat2 = wlRat2, ptRat2 = ptRat2, avgor2 = avgor2, avgdr2 = avgdr2, avgast2 = avgast2, avgto2 = avgto2, avgstl2 = avgstl2, avgblk2 = avgblk2, avgpf2 = avgpf2)

firstteam <- vector()
secondteam <- vector()
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
for(i in 1:length(teams2012)) {
  for(j in (i + 1):length(teams2012)) {
    if(j <= length(teams2012) & i != j) {
      firstteam = rbind(firstteam, teams2012[i])
      secondteam = rbind(secondteam, teams2012[j])
      fgPer1 = rbind(fgPer1, stats$fgPer[teams2012[i] - 1100])
      fg3Per1 = rbind(fg3Per1, stats$fg3Per[teams2012[i] - 1100])
      ftPer1 = rbind(ftPer1, stats$ftPer[teams2012[i] - 1100])
      wlRat1 = rbind(wlRat1, stats$wlRat[teams2012[i] - 1100])
      ptRat1 = rbind(ptRat1, stats$ptRat[teams2012[i] - 1100])
      avgor1 = rbind(avgor1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgdr1 = rbind(avgdr1, stats$totaldr[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgast1 = rbind(avgast1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgto1 = rbind(avgto1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgstl1 = rbind(avgstl1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgblk1 = rbind(avgblk1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      avgpf1 = rbind(avgpf1, stats$totalor[teams2012[i] - 1100]/(stats$totalwins[teams2012[i] - 1100] + stats$totallosses[teams2012[i] - 1100]))
      fgPer2 = rbind(fgPer2, stats$fgPer[teams2012[j] - 1100])
      fg3Per2 = rbind(fg3Per2, stats$fg3Per[teams2012[j] - 1100])
      ftPer2 = rbind(ftPer2, stats$ftPer[teams2012[j] - 1100])
      wlRat2 = rbind(wlRat2, stats$wlRat[teams2012[j] - 1100])
      ptRat2 = rbind(ptRat2, stats$ptRat[teams2012[j] - 1100])
      avgor2 = rbind(avgor2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgdr2 = rbind(avgdr2, stats$totaldr[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgast2 = rbind(avgast2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgto2 = rbind(avgto2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgstl2 = rbind(avgstl2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgblk2 = rbind(avgblk2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
      avgpf2 = rbind(avgpf2, stats$totalor[teams2012[j] - 1100]/(stats$totalwins[teams2012[j] - 1100] + stats$totallosses[teams2012[j] - 1100]))
    }
  }
}
matchups2012 <- data.frame(team1 = firstteam, team2 = secondteam, fgPer1 = fgPer1, fg3Per1 = fg3Per1, ftPer1 = ftPer1, wlRat1 = wlRat1, ptRat1 = ptRat1, avgor1 = avgor1, avgdr1 = avgdr1, avgast1 = avgast1, avgto1 = avgto1, avgstl1 = avgstl1, avgblk1 = avgblk1, avgpf1 = avgpf1, fgPer2 = fgPer2, fg3Per2 = fg3Per2, ftPer2 = ftPer2, wlRat2 = wlRat2, ptRat2 = ptRat2, avgor2 = avgor2, avgdr2 = avgdr2, avgast2 = avgast2, avgto2 = avgto2, avgstl2 = avgstl2, avgblk2 = avgblk2, avgpf2 = avgpf2)

firstteam <- vector()
secondteam <- vector()
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
for(i in 1:length(teams2013)) {
  for(j in (i + 1):length(teams2013)) {
    if(j <= length(teams2013) & i != j) {
      firstteam = rbind(firstteam, teams2013[i])
      secondteam = rbind(secondteam, teams2013[j])
      fgPer1 = rbind(fgPer1, stats$fgPer[teams2013[i] - 1100])
      fg3Per1 = rbind(fg3Per1, stats$fg3Per[teams2013[i] - 1100])
      ftPer1 = rbind(ftPer1, stats$ftPer[teams2013[i] - 1100])
      wlRat1 = rbind(wlRat1, stats$wlRat[teams2013[i] - 1100])
      ptRat1 = rbind(ptRat1, stats$ptRat[teams2013[i] - 1100])
      avgor1 = rbind(avgor1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgdr1 = rbind(avgdr1, stats$totaldr[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgast1 = rbind(avgast1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgto1 = rbind(avgto1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgstl1 = rbind(avgstl1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgblk1 = rbind(avgblk1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      avgpf1 = rbind(avgpf1, stats$totalor[teams2013[i] - 1100]/(stats$totalwins[teams2013[i] - 1100] + stats$totallosses[teams2013[i] - 1100]))
      fgPer2 = rbind(fgPer2, stats$fgPer[teams2013[j] - 1100])
      fg3Per2 = rbind(fg3Per2, stats$fg3Per[teams2013[j] - 1100])
      ftPer2 = rbind(ftPer2, stats$ftPer[teams2013[j] - 1100])
      wlRat2 = rbind(wlRat2, stats$wlRat[teams2013[j] - 1100])
      ptRat2 = rbind(ptRat2, stats$ptRat[teams2013[j] - 1100])
      avgor2 = rbind(avgor2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgdr2 = rbind(avgdr2, stats$totaldr[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgast2 = rbind(avgast2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgto2 = rbind(avgto2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgstl2 = rbind(avgstl2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgblk2 = rbind(avgblk2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
      avgpf2 = rbind(avgpf2, stats$totalor[teams2013[j] - 1100]/(stats$totalwins[teams2013[j] - 1100] + stats$totallosses[teams2013[j] - 1100]))
    }
  }
}
matchups2013 <- data.frame(team1 = firstteam, team2 = secondteam, fgPer1 = fgPer1, fg3Per1 = fg3Per1, ftPer1 = ftPer1, wlRat1 = wlRat1, ptRat1 = ptRat1, avgor1 = avgor1, avgdr1 = avgdr1, avgast1 = avgast1, avgto1 = avgto1, avgstl1 = avgstl1, avgblk1 = avgblk1, avgpf1 = avgpf1, fgPer2 = fgPer2, fg3Per2 = fg3Per2, ftPer2 = ftPer2, wlRat2 = wlRat2, ptRat2 = ptRat2, avgor2 = avgor2, avgdr2 = avgdr2, avgast2 = avgast2, avgto2 = avgto2, avgstl2 = avgstl2, avgblk2 = avgblk2, avgpf2 = avgpf2)

firstteam <- vector()
secondteam <- vector()
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
for(i in 1:length(teams2014)) {
  for(j in (i + 1):length(teams2014)) {
    if(j <= length(teams2014) & i != j) {
      firstteam = rbind(firstteam, teams2014[i])
      secondteam = rbind(secondteam, teams2014[j])
      fgPer1 = rbind(fgPer1, stats$fgPer[teams2014[i] - 1100])
      fg3Per1 = rbind(fg3Per1, stats$fg3Per[teams2014[i] - 1100])
      ftPer1 = rbind(ftPer1, stats$ftPer[teams2014[i] - 1100])
      wlRat1 = rbind(wlRat1, stats$wlRat[teams2014[i] - 1100])
      ptRat1 = rbind(ptRat1, stats$ptRat[teams2014[i] - 1100])
      avgor1 = rbind(avgor1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgdr1 = rbind(avgdr1, stats$totaldr[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgast1 = rbind(avgast1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgto1 = rbind(avgto1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgstl1 = rbind(avgstl1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgblk1 = rbind(avgblk1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      avgpf1 = rbind(avgpf1, stats$totalor[teams2014[i] - 1100]/(stats$totalwins[teams2014[i] - 1100] + stats$totallosses[teams2014[i] - 1100]))
      fgPer2 = rbind(fgPer2, stats$fgPer[teams2014[j] - 1100])
      fg3Per2 = rbind(fg3Per2, stats$fg3Per[teams2014[j] - 1100])
      ftPer2 = rbind(ftPer2, stats$ftPer[teams2014[j] - 1100])
      wlRat2 = rbind(wlRat2, stats$wlRat[teams2014[j] - 1100])
      ptRat2 = rbind(ptRat2, stats$ptRat[teams2014[j] - 1100])
      avgor2 = rbind(avgor2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgdr2 = rbind(avgdr2, stats$totaldr[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgast2 = rbind(avgast2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgto2 = rbind(avgto2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgstl2 = rbind(avgstl2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgblk2 = rbind(avgblk2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
      avgpf2 = rbind(avgpf2, stats$totalor[teams2014[j] - 1100]/(stats$totalwins[teams2014[j] - 1100] + stats$totallosses[teams2014[j] - 1100]))
    }
  }
}
matchups2014 <- data.frame(team1 = firstteam, team2 = secondteam, fgPer1 = fgPer1, fg3Per1 = fg3Per1, ftPer1 = ftPer1, wlRat1 = wlRat1, ptRat1 = ptRat1, avgor1 = avgor1, avgdr1 = avgdr1, avgast1 = avgast1, avgto1 = avgto1, avgstl1 = avgstl1, avgblk1 = avgblk1, avgpf1 = avgpf1, fgPer2 = fgPer2, fg3Per2 = fg3Per2, ftPer2 = ftPer2, wlRat2 = wlRat2, ptRat2 = ptRat2, avgor2 = avgor2, avgdr2 = avgdr2, avgast2 = avgast2, avgto2 = avgto2, avgstl2 = avgstl2, avgblk2 = avgblk2, avgpf2 = avgpf2)

#Decision tree
library(rpart)
fit <- rpart(won ~ fgPer1 + fg3Per1 + ftPer1 + wlRat1 + ptRat1 + avgor1 + avgdr1 + avgast1 + avgto1 + avgstl1 + avgblk1 + avgpf1 + fgPer2 + fg3Per2 + ftPer2 + wlRat2 + ptRat2 + avgor2 + avgdr2 + avgast2 + avgto2 + avgstl2 + avgblk2 + avgpf2, data = train)
Prediction2011 <- predict(fit, matchups2011)
Prediction2012 <- predict(fit, matchups2012)
Prediction2013 <- predict(fit, matchups2013)
Prediction2014 <- predict(fit, matchups2014)

matchups2011$id <- paste("2011", matchups2011$team1, matchups2011$team2, sep = "_")
matchups2012$id <- paste("2012", matchups2012$team1, matchups2012$team2, sep = "_")
matchups2013$id <- paste("2013", matchups2013$team1, matchups2013$team2, sep = "_")
matchups2014$id <- paste("2014", matchups2014$team1, matchups2014$team2, sep = "_")

submit2011 <- data.frame(id = matchups2011$id, pred = Prediction2011)
submit2012 <- data.frame(id = matchups2012$id, pred = Prediction2012)
submit2013 <- data.frame(id = matchups2013$id, pred = Prediction2013)
submit2014 <- data.frame(id = matchups2014$id, pred = Prediction2014)
submit <- rbind(submit2011, submit2012, submit2013, submit2014)
write.csv(submit, file = "historic.csv", row.names = FALSE)
