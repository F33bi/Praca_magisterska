library(TSdist)
library(caret)
library(e1071)
library(cluster)
library(TSclust)
library(dtw)

load("D:/Magisterka/wd/car_data.RData")

My.KMedoids = function(data, data.classes, k, distance, ...) {
  n = length(data.classes)  
  start = Sys.time()
  
  predicted = KMedoids(data = data, k = k, ground.truth = data.classes, 
                       distance = distance, ...)$clustering
  # print(predicted)
  # print(data.classes)
  # print(table(predicted, data.classes))
  matched = matchClasses(table(predicted, data.classes), "exact", verbose = FALSE)
  predicted_matched = as.integer(matched[as.character(predicted)])
  
  agreement = sum(predicted_matched == data.classes)/n
  diss = matrix(data = 0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in i:n) {
      diss[i, j] = TSDistances(as.numeric(data[i, ]), as.numeric(data[j, ]), distance = distance, ...)
      diss[j, i] = diss[i, j]
    }
  }
  silhouette = summary(silhouette(as.integer(predicted), diss))
  
  time = Sys.time() - start
  
  list(predicted = predicted_matched,
       time = time,
       agreement = agreement,
       silhouette = silhouette$avg.width)
}

load("D:/Magisterka/wd/data.RData")
load("D:/Magisterka/wd/My_KMedoids_function.RData")

SAX.km = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k=2, 
                     "mindist.sax", w=2)
Euclidean.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
                           "euclidean")
CCor.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
                      "ccor")
DTW.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2,
                     "dtw")
ACF.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
                     "acf", lag.max = 5)
CID.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
                     "cid")
Fourier.km = My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
                         "fourier")

setwd("D:/Magisterka/wd")
save(SAX.km, Euclidean.km, CCor.km, DTW.km, ACF.km, CID.km, Fourier.km, file = "KMedoids_results.RData")

setwd("D:/Magisterka/wd")
save(My.KMedoids, file = "My_KMedoids_function.RData")

library(dtwclust)
# tsclust {dtwclust}, sprawdzić czym jest type = "tadpole"
