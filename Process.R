train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$fgPerDiff <- 0
train$fg3PerDiff <- 0
train$ftPerDiff <- 0
train$wlRatDiff <- 0
train$ptRatDiff <- 0
train$avgorDiff <- 0
train$avgdrDiff <- 0
train$avgastDiff <- 0
train$avgtoDiff <- 0
train$avgstlDiff <- 0
train$avgblkDiff <- 0
train$avgpfDiff <- 0
train$seedDiff <- 0

for(i in 1:length(train$team1)) {
  train$fgPerDiff[i] = train$fgPer1[i] - train$fgPer2[i]
  train$fg3PerDiff[i] = train$fg3Per1[i] - train$fg3Per2[i]
  train$ftPerDiff[i] = train$ftPer1[i] - train$ftPer2[i]
  train$wlRatDiff[i] = train$wlRat1[i] - train$wlRat2[i]
  train$ptRatDiff[i] = train$ptRat1[i] - train$ptRat2[i]
  train$avgorDiff[i] = train$avgor1[i] - train$avgor2[i]
  train$avgdrDiff[i] = train$avgdr1[i] - train$avgdr2[i]
  train$avgastDiff[i] = train$avgast1[i] - train$avgast2[i]
  train$avgtoDiff[i] = train$avgto1[i] - train$avgto2[i]
  train$avgstlDiff[i] = train$avgstl1[i] - train$avgstl2[i]
  train$avgblkDiff[i] = train$avgblk1[i] - train$avgblk2[i]
  train$avgpfDiff[i] = train$avgpf1[i] - train$avgpf2[i]
  train$seedDiff[i] = train$seed1[i] - train$seed2[i]
}

test$fgPerDiff <- 0
test$fg3PerDiff <- 0
test$ftPerDiff <- 0
test$wlRatDiff <- 0
test$ptRatDiff <- 0
test$avgorDiff <- 0
test$avgdrDiff <- 0
test$avgastDiff <- 0
test$avgtoDiff <- 0
test$avgstlDiff <- 0
test$avgblkDiff <- 0
test$avgpfDiff <- 0
test$seedDiff <- 0

for(i in 1:length(test$team1)) {
  test$fgPerDiff[i] = test$fgPer1[i] - test$fgPer2[i]
  test$fg3PerDiff[i] = test$fg3Per1[i] - test$fg3Per2[i]
  test$ftPerDiff[i] = test$ftPer1[i] - test$ftPer2[i]
  test$wlRatDiff[i] = test$wlRat1[i] - test$wlRat2[i]
  test$ptRatDiff[i] = test$ptRat1[i] - test$ptRat2[i]
  test$avgorDiff[i] = test$avgor1[i] - test$avgor2[i]
  test$avgdrDiff[i] = test$avgdr1[i] - test$avgdr2[i]
  test$avgastDiff[i] = test$avgast1[i] - test$avgast2[i]
  test$avgtoDiff[i] = test$avgto1[i] - test$avgto2[i]
  test$avgstlDiff[i] = test$avgstl1[i] - test$avgstl2[i]
  test$avgblkDiff[i] = test$avgblk1[i] - test$avgblk2[i]
  test$avgpfDiff[i] = test$avgpf1[i] - test$avgpf2[i]
  test$seedDiff[i] = test$seed1[i] - test$seed2[i]
}

#Stepwise linear model
library(MASS)
lm.lower <- lm(won ~ 1, data = train)
lm.upper <- lm(won ~ fgPerDiff + fg3PerDiff + ftPerDiff + wlRatDiff + ptRatDiff +
                 avgorDiff + avgdrDiff + avgastDiff + avgtoDiff + avgstlDiff +
                 avgblkDiff + avgpfDiff + seedDiff, data = train)
lm.final <- stepAIC(lm.lower, direction = "forward", scope = list(lower = lm.lower, upper = lm.upper))
Prediction <- predict(lm.final, test)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction)
for(i in 1:length(submit$id)) {
  if(submit$pred[i] < 0) {
    submit$pred[i] = 0
  }
  if(submit$pred[i] > 1) {
    submit$pred[i] = 1
  }
}
write.csv(submit, "stepwiselinear.csv", quote = FALSE, row.names = FALSE)

#Decision tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(won ~ ptRatDiff + seedDiff, data = train)
fancyRpartPlot(fit)
Prediction <- predict(fit, test)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction)
write.csv(submit, "decisiontreediff.csv", quote = FALSE, row.names = FALSE)

#Basic linear model
lm.basic <- lm(won ~ ptRatDiff + seedDiff, data = train)
Prediction <- predict(lm.basic, test)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction)
for(i in 1:length(submit$id)) {
  if(submit$pred[i] < 0) {
    submit$pred[i] = 0
  }
  if(submit$pred[i] > 1) {
    submit$pred[i] = 1
  }
}
write.csv(submit, "basiclinear.csv", quote = FALSE, row.names = FALSE)

#Forest of conditional inference trees
library(party)
fit <- cforest(won ~ ptRatDiff + seedDiff, data = train)
Prediction <- predict(fit, test, OOB = TRUE)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction[,1])
write.csv(submit, "conditionaltreediff.csv", quote = FALSE, row.names = FALSE)
