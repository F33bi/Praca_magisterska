library(TSdist)
library(caret)

# dane znormalizowane
data.train = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TRAIN.txt')
data.test = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TEST.txt')

My.oneNN = function(train, train.classes, test, test.classes, distance, ...) {
  start = Sys.time()
  predicted = OneNN(train, train.classes, test, test.classes, distance, ...)
  time = Sys.time() - start
  
  conf.matrix = confusionMatrix(as.factor(predicted$classes), as.factor(test.classes))
  
  list(predicted = predicted$classes,
       time = time,
       accuracy = as.numeric(conf.matrix$overall[1]),
       sensitivity = as.numeric(conf.matrix$byClass[1]),
       specificity = as.numeric(conf.matrix$byClass[2]),
       precision = as.numeric(conf.matrix$byClass[5]),
       recall = as.numeric(conf.matrix$byClass[6]))
}

SAX = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               "mindist.sax", w = 2)
Euclidean = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
                     "euclidean")
CCor = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
                "ccor")
DTW = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               "dtw")
ACF = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               "acf", lag.max = 5)
CID = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               "cid")

setwd('/Volumes/NO NAME/Magisterka/wd')
save(data.train, data.test, Euclidean, CCor, SAX, DTW, ACF, CID,  My.oneNN, file = "1NN.RData")

# wizulizacja macierzy odmienności dist() - {factoextra}, raczej w innym pliku
# spisać pytania badawcze

# step.pattern = symmetric1, symmetric2, asymmetric
# dist.method = "Euclidean", "correlation"
