library(TSdist)
library(caret)
library(e1071)

load("D:/Magisterka/wd/data.RData")
# load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')
# load('/Volumes/NO NAME/Magisterka/wd/car_data.RData')

# KMed -------------------------------------------------------------------------------------------------------------
time.vec = NULL
lag.max.vec = 1:15
for (lag in lag.max.vec) {
  print(lag)
  acf.method = My.KMedoids(data.full[, -1], data.full[, 1], k = 2,
                           "acf", lag.max = lag)
  accuracy.vec = append(accuracy.vec, acf.method$accuracy)
  time.vec = append(time.vec, acf.method$time)
}

setwd("D:/Magisterka/wd")
save(lag.max.vec, accuracy.vec, time.vec, file = "ACF_vs_lag_computers_KMed.RData")

# 1NN --------------------------------------------------------------------------------------------------------------
accuracy.vec = NULL
time.vec = NULL
lag.max.vec = 1:15
for (lag in lag.max.vec) {
  print(lag)
  acf.method = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1],
                        distance = "acf", lag.max = lag)
  accuracy.vec = append(accuracy.vec, acf.method$accuracy)
  time.vec = append(time.vec, acf.method$time)
}

setwd("D:/Magisterka/wd")
save(lag.max.vec, accuracy.vec, time.vec, file = "ACF_vs_lag_ecg_1NN.RData")

# ------------------------------------------------------------------------------------------------------------------

# load("D:/Magisterka/wd/ACF_vs_lag_car_1NN.RData")
plot(lag.max.vec, accuracy.vec, type = "l", main = "Accuracy vs lag.max in ACF method", 
     xlab = "lag.max", ylab = "Accuracy", lty = "dotted", col = "darkgrey")
points(lag.max.vec, accuracy.vec, pch = 16, col = "black")
text(lag.max.vec, accuracy.vec - 0.004, 
     labels = paste0(round(time.vec, 2), " s"), 
     cex = 0.6, col = "blue")
