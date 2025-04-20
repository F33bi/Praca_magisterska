library(TSdist)
library(caret)

load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')

data.train = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TRAIN.txt')
data.test = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TEST.txt')
data.full = rbind(data.train, data.test)

One.NN.random.split = function(data, data.classes, distance, ...) {
  accuracy.vec = NULL
  for (i in 1:20) {
    set.seed(i)
    train.index = sample(1:500, 250)
    train = data[train.index, ]
    train.classes = data.classes[train.index]
    test = data[-c(train.index), ]
    test.classes = data.classes[-c(train.index)]
    accuracy.vec[i] = My.oneNN(train, train.classes, test, test.classes, distance, ...)$accuracy
  }
  return(accuracy.vec)
}

SAX.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "mindist.sax", w = 2)
ACF.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "acf", lag.max = 6)
CCor.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "ccor")
CID.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "cid")
Euclidean.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "euclidean")
DTW.acc = One.NN.random.split(data.full[, -1], data.full[, 1], "dtw") # 15,2 h

setwd('/Volumes/NO NAME/Magisterka/wd')
save(SAX.acc, ACF.acc, CCor.acc, CID.acc, Euclidean.acc, DTW.acc, 
     One.NN.random.split, My.oneNN,
     file = "random_split.RData")

load('/Volumes/NO NAME/Magisterka/wd/random_split.RData')

boxplot(cbind(SAX.acc, ACF.acc, CCor.acc, CID.acc, Euclidean.acc, DTW.acc),
        main = "Wykresy pudełkowe dla wybranych metod", 
        ylab = "Accuracy", names = c("SAX", "ACF", "CCor", "CID", "Euclidean", "DTW"),
        col = terrain.colors(6))
