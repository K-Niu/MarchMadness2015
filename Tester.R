train <- read.csv("train.csv")
tourney <- read.csv("tourney_compact_results.csv")
tourney11to14 <- tourney[tourney$season == 2011 | tourney$season == 2012 | tourney$season == 2013 | tourney$season == 2014,]
predictions <- read.csv("conditiontreeless.csv")

logloss <- 0
for(i in 1:length(tourney11to14$season)) {
  teams = c(tourney11to14$wteam[i], tourney11to14$lteam[i])
  teams = sort(teams)
  id = paste(tourney11to14$season[i], teams[1], teams[2], sep = "_")
  matchup = predictions[predictions$id == id,]
  prob = matchup$pred[1]
  result = 0
  if(tourney11to14$wteam[i] < tourney11to14$lteam[i]) {
    result = 1
  }
  if(tourney11to14$season == 2011 & (tourney11to14$lteam[i] == 1106 | tourney11to14$lteam[i] == 1412 | tourney11to14$lteam[i] == 1114 | tourney11to14$lteam[i] == 1425)) {
    logloss = logloss
  } else if (tourney11to14$season == 2012 & (tourney11to14$lteam[i] == 1290 | tourney11to14$lteam[i] == 1249 | tourney11to14$lteam[i] == 1233 | tourney11to14$lteam[i] == 1143)) {
    logloss = loglos
  } else if (tourney11to14$season == 2013 & (tourney11to14$lteam[i] == 1292 | tourney11to14$lteam[i] == 1251 | tourney11to14$lteam[i] == 1129 | tourney11to14$lteam[i] == 1254)) {
    logloss = logloss
  } else if (tourney11to14$season == 2014 & (tourney11to14$lteam[i] == 1462 | tourney11to14$lteam[i] == 1291 | tourney11to14$lteam[i] == 1234 | tourney11to14$lteam[i] == 1411)) {
    logloss = logloss
  } else {
    logloss = logloss + result*log(prob) + (1 - result)*log(1 - prob)
  }
}

logloss = -(1/length(tourney11to14$season))*logloss