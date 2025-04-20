library(TSdist)
library(caret)

load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')

accuracy.vec = NULL
time.vec = NULL
lag.max.vec = 1:15
for (lag in lag.max.vec) {
  acf.method = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1],
                        "acf", lag.max = lag)
  accuracy.vec = append(accuracy.vec, acf.method$accuracy)
  time.vec = append(time.vec, acf.method$time)
}

plot(lag.max.vec, accuracy.vec, type = "l", main = "Accuracy vs lag.max in ACF method", 
     xlab = "lag.max", ylab = "Accuracy", lty = "dotted", col = "darkgrey", ylim = c(0.56, 0.615))
points(lag.max.vec, accuracy.vec, pch = 16, col = "black")
text(lag.max.vec, accuracy.vec - 0.005, 
     labels = paste0(round(time.vec, 2), " s"), 
     cex = 0.6, col = "blue")
