library(TSdist)
library(caret)

load("D:/Magisterka/wd/car_data.RData")
load("D:/Magisterka/wd/data.RData")
load("D:/Magisterka/wd/ecg_data.RData")


My.oneNN = function(train, train.classes, test, test.classes, distance, ...) {
  start = Sys.time()
  
  predicted = OneNN(train, train.classes, test, test.classes, distance, ...)
  
  # conf.matrix = confusionMatrix(as.factor(predicted$classes), as.factor(test.classes))
  table = table(as.factor(predicted$classes), as.factor(test.classes))
  levels = unique(test.classes)
  
  time = Sys.time() - start
  
  list(predicted = predicted$classes,
       time = time,
       accuracy = sum(predicted$classes == test.classes)/length(test.classes),
       recall = recall(table, relevant = "-1"),
       precision = precision(table, relevant = "-1"),
       f1 = F_meas(table, relevant = "-1")
       )
}

My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
         "mindist.sax", w = 2)

My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
         "mindist.sax", w = 2)
# --------------------------------------------------------------------------------------------------------------------------

SAX = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
               "mindist.sax", w = 2)
Euclidean = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
                     "euclidean")
CCor = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
                "ccor")
DTW = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
               "dtw")
ACF = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
               "acf", lag.max = 5)
CID = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1], 
               "cid")
Fourier = My.oneNN(ecg.train[, -1], ecg.train[, 1], ecg.test[, -1], ecg.test[, 1],
                   "fourier")

df.ecg.1nn = rbind(ACF, DTW, Euclidean, SAX, CCor, CID, Fourier)
df.ecg.1nn = df.ecg.1nn[, -1]

colnames(df.ecg.1nn)[1] = "time (sec)"

setwd("D:/Magisterka/wd")
save(df.ecg.1nn, file = "df_1nn_ecg.RData")

# setwd("D:/Magisterka/wd")
# save(Euclidean, CCor, SAX, DTW, ACF, CID, Fourier, file = "1NN.RData")

# wizulizacja macierzy odmienności dist() - {factoextra}, raczej w innym pliku
# spisać pytania badawcze

# step.pattern = symmetric1, symmetric2, asymmetric
# dist.method = "Euclidean", "correlation"
