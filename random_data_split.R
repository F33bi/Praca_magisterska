library(TSdist)
library(caret)

# load("D:/Magisterka/wd/car_data.RData")
# load("D:/Magisterka/wd/data.RData")
load("D:/Magisterka/wd/ecg_data.RData")

# load("D:/Magisterka/wd/computers_random_split.RData")


One.NN.random.split = function(data, data.classes, distance, ...) {
  n = nrow(data)
  accuracy.vec = NULL
  for (i in 1:20) {
    print(i)
    set.seed(i)
    train.index = sample(1:n, n/2)
    train = data[train.index, ]
    train.classes = data.classes[train.index]
    test = data[-c(train.index), ]
    test.classes = data.classes[-c(train.index)]
    accuracy.vec[i] = My.oneNN(train, train.classes, test, test.classes, distance, ...)$accuracy
  }
  return(accuracy.vec)
}

SAX.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "mindist.sax", w = 30, alpha = 12)
ACF.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "acf", lag.max = 5)
CCor.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "ccor")
CID.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "cid")
Euclidean.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "euclidean")
DTW.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "dtw") # computers - 15,2 h
Fourier.acc = One.NN.random.split(ecg.full[, -1], ecg.full[, 1], "fourier")

setwd("D:/Magisterka/wd")
save(SAX.acc, ACF.acc, CCor.acc, CID.acc, Euclidean.acc, DTW.acc, Fourier.acc, 
     file = "ecg_random_split.RData")

# load('/Volumes/NO NAME/Magisterka/wd/random_split.RData')

boxplot(cbind(SAX.acc, ACF.acc, CCor.acc, CID.acc, Euclidean.acc, DTW.acc, Fourier.acc),
        main = "Wykresy pudełkowe dla danych car", 
        ylab = "Accuracy", names = c("SAX", "ACF", "CCor", "CID", "Euclidean", "DTW", "Fourier"),
        col = rainbow(7))

car.random.split.df = rbind(c(mean(SAX.acc), sd(SAX.acc)),
                            c(mean(ACF.acc), sd(ACF.acc)),
                            c(mean(CCor.acc), sd(CCor.acc)),
                            c(mean(CID.acc), sd(CID.acc)),
                            c(mean(Euclidean.acc), sd(Euclidean.acc)),
                            c(mean(DTW.acc), sd(DTW.acc)),
                            c(mean(Fourier.acc), sd(Fourier.acc)))
car.random.split.df = as.data.frame(car.random.split.df)
rownames(car.random.split.df) = c("SAX", "ACF", "CCor", "CID", "Euclidean", "DTW", "Fourier")
colnames(car.random.split.df) = c("Średnia", "Odchylenie standardowe")

# setwd("D:/Magisterka/wd")
# save(car.random.split.df,
#      file = "car_random_split_df.RData")
