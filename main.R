rm(list=ls())
source("config.R")
source("strategies.R")

library(forecast)

myts <- window(sunspot.year,start=1700,end=1988-11)
future <- as.numeric(window(sunspot.year,start=1988-11,1988))

# Forecasting horizon
H <- 12

# Cross validation procedures
control <- strategy_control("ts-cv",train.percentage = 0.7, n.fold=5)

# Learners
bst1 <- strategy_learner(name = "BST", interactions=1, nu = 0.3, max.mstop = 500)
bst2 <- strategy_learner(name = "BST", interactions=2, nu = 0.3, max.mstop = 500)

# KNN NOT AVAILABLE AT THIS STAGE
# knn <-  strategy_learner(name = "KNN")

mlp <-  strategy_learner(name = "MLP", set.hidden = c(0,1,2,3,5), set.decay = c(0.005,0.01,0.05,0.1,0.2,0.3), nb.runs = 1, maxiter = 100, drop.linbias = F)
lin <-  strategy_learner(name = "MLP", set.hidden = 0, set.decay = 0, nb.runs = 1, drop.linbias = F)

# Recursive forecasts
resultsREC <- recursive(time.series = myts, H = H, set.embedding = c(1,2,3,4,5), objectives = details$objectives, learner = mlp, control = control)
frec <- as.numeric(resultsREC$forecasts)

# Direct forecasts
resultsDIR <- direct(time.series = myts, H = H, set.embedding = c(1,2,3,4,5), objectives = details$objectives, learner = mlp, control = control)
fdir <- as.numeric(resultsDIR$forecasts)

# Boosted AR forecasts
resultsRECT <- rectify(time.series = myts, H = H, embeddings.base = c(1,2,3,4,5), embeddings.rect = c(1,2,3,4,5), objectives = details$objectives, learner.bmodel = lin, learner = bst1, control = control)
fbase <- as.numeric(resultsRECT$base.forecasts)
frect <- as.numeric(resultsRECT$forecasts)

# Auto.arima forecasts
farima <- as.numeric(forecast(auto.arima(myts), h = 12)$mean)


# Base AR forecasts in red
# Boosting AR forecasts in blue
# Real future in green
matplot(cbind(fbase, frect, future), col = c("red", "blue", "green"), type = 'l', lty = 1)

# Comparing recursive, direct, arima and boosted ar forecasts
mat <- cbind(future, frec, fdir, farima, frect)
mycol <- rainbow(ncol(mat))
matplot(mat, col = mycol, type = 'l', lty = 1)
