library(cluster)
library(e1071)

load('/Volumes/NO NAME/Magisterka/wd/ecg_data.RData')

My.AGNES = function(data, distance, k, ...) {
  start = Sys.time()
  
  nrow = nrow(data)
  
  diss.matrix = matrix(nrow = nrow, ncol = nrow, data = 0)
  
  for (first_series in 1:nrow) {
    print(first_series)
    for (second_series in first_series:nrow) {
      diss.matrix[first_series, second_series] = 
        TSDistances(as.numeric(data[first_series, -1]), as.numeric(data[second_series, -1]), 
                    distance = distance, ...)
      diss.matrix[second_series, first_series] = diss.matrix[first_series, second_series]
    }
  }
  
  agnes = agnes(diss.matrix, diss = TRUE)
  predicted = cutree(agnes, k)
  # print(predicted)
  # print(table(predicted, data[, 1]))
  matched = matchClasses(table(predicted, data[, 1]), "exact", verbose = FALSE)
  predicted_matched = as.integer(matched[as.character(predicted)])
  
  agreement = sum(predicted_matched == data[, 1])/nrow
  
  silhouette = summary(silhouette(as.integer(predicted), diss.matrix))
  
  time = Sys.time() - start
  return(list(predicted = predicted_matched, 
              time = time,
              agreement = agreement,
              silhouette = silhouette$avg.width))
}

pred = My.AGNES(ecg.full, "euclidean", k = 2)$predicted
real = ecg.full[, 1]

table(pred, real)
