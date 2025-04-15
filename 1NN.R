data.train = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TRAIN.txt')
data.test = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TEST.txt')

# ------------------------------------------------------------------------------
start = Sys.time()
predicted.euclidean = OneNN(data.train[, -1], data.train[, 1], 
                                    data.test[, -1], data.test[, 1], 
                                    distance = "euclidean")
time.euclidean = Sys.time() - start
accuracy.euclidean = sum(predicted.euclidean$classes == data.test[, 1])/length(data.test[, 1])

# ------------------------------------------------------------------------------
start = Sys.time()
predicted.ccor = OneNN(data.train[, -1], data.train[, 1], 
                               data.test[, -1], data.test[, 1], 
                               distance = "ccor")
time.ccor = Sys.time() - start
accuracy.ccor = sum(predicted.ccor$classes == data.test[, 1])/length(data.test[, 1])

# ------------------------------------------------------------------------------
start = Sys.time()
predicted.sax = OneNN(data.train[, -1], data.train[, 1], 
                       data.test[, -1], data.test[, 1], 
                       distance = "mindist.sax", w = 5)
time.sax = Sys.time() - start
accuracy.sax = sum(predicted.sax$classes == data.test[, 1])/length(data.test[, 1])

# ------------------------------------------------------------------------------
start = Sys.time()
# step.pattern = symmetric1, symmetric2, asymmetric
# dist.method = "Euclidean", "correlation"
predicted.dtw = OneNN(data.train[, -1], data.train[, 1], 
                      data.test[, -1], data.test[, 1], 
                      distance = "dtw")
time.dtw = Sys.time() - start
accuracy.dtw = sum(predicted.dtw$classes == data.test[, 1])/length(data.test[, 1])

list.euclidean = list(predicted = predicted.euclidean, time = time.euclidean, 
                      accuracy = accuracy.euclidean)
list.ccor = list(predicted = predicted.ccor, time = time.ccor, 
                      accuracy = accuracy.ccor)
list.sax = list(predicted = predicted.sax, time = time.sax, 
                accuracy = accuracy.sax)
list.dtw = list(predicted = predicted.dtw, time = time.dtw, 
                accuracy = accuracy.dtw)

setwd('/Volumes/NO NAME/Magisterka/wd')
save(data.train, data.test, list.euclidean, list.ccor, list.sax, list.dtw, file = "1NN.RData")
