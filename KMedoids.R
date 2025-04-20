library(TSdist)

data.train = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TRAIN.txt')
data.test = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TEST.txt')
data.full = rbind(data.train, data.test)

# ------------------------------------------------------------------------------
start = Sys.time()
predicted.euclidean = KMedoids(data.full[, -1], data.full[, 1], k=2, "euclidean")
time.euclidean = Sys.time() - start
accuracy.euclidean = sum(predicted.euclidean$clustering == data.full[, 1])/length(data.full[, 1])

# ------------------------------------------------------------------------------
start = Sys.time()
predicted.ccor = KMedoids(data.full[, -1], data.full[, 1], k=2, "ccor")
time.ccor = Sys.time() - start
accuracy.ccor = sum(predicted.ccor$clustering == data.full[, 1])/length(data.full[, 1])

library(e1071)
# matchClasses {e1071}

library(dtwclust)
# tsclust {dtwclust}, sprawdzić czym jest type = "tadpole"
