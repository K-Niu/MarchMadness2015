train <- read.csv("train.csv")
test <- read.csv("test.csv")

cormat <- cor(train)
correlated <- vector()
for(i in 1:length(rownames(cormat))) {
  for(j in (i + 1):length(rownames(cormat))) {
    if(j <= length(rownames(cormat)) & i != j) {
      if(abs(cormat[i, j]) > 0.2) {
        correlated = cbind(correlated, paste(rownames(cormat)[i], rownames(cormat)[j], sep = "-"))
      }
    }
  }
}

#Decision tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(won ~ ftPer1 + avgto1 + seed1 + ftPer2 + avgto2 + seed2, data = train)
fancyRpartPlot(fit)
Prediction <- predict(fit, test)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction)
write.csv(submit, "evenlessvariables.csv", quote = FALSE, row.names = FALSE)

#Forest of conditional inference trees
library(party)
fit <- cforest(won ~ won ~ ptRat1 + seed1 + ptRat2 + seed2, data = train)
Prediction <- predict(fit, test, OOB = TRUE)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction[,1])
write.csv(submit, "conditiontreeless2.csv", quote = FALSE, row.names = FALSE)

#Multiple linear regression
fit <- lm(won ~ ptRat1 + seed1 + ptRat2 + seed2, data = train)
summary(fit)
Prediction <- predict(fit, test)
test$id <- paste(test$season, test$team1, test$team2, sep = "_")
submit <- data.frame(id = test$id, pred = Prediction)
write.csv(submit, "multipleregression.csv", quote = FALSE, row.names = FALSE)
