library(TSdist)
library(caret)
library(zoo)
library(forecast)
library(cluster)
library(e1071)

My.oneNN.car = function(train, train.classes, test, test.classes, distance, ...) {
  start = Sys.time()
  
  predicted = OneNN(train, train.classes, test, test.classes, distance, ...)
  
  # conf.matrix = confusionMatrix(as.factor(predicted$classes), as.factor(test.classes))
  table = table(as.factor(predicted$classes), as.factor(test.classes))
  levels = unique(test.classes)
  
  time = Sys.time() - start
  
  list(predicted = predicted$classes,
       time = time,
       accuracy = sum(predicted$classes == test.classes)/length(test.classes),
       recall = (recall(table, relevant = "1") + recall(table, relevant = "2") +
                   recall(table, relevant = "3") + recall(table, relevant = "4"))/4,
       precision = (precision(table, relevant = "1") + precision(table, relevant = "2") +
                      precision(table, relevant = "3") + precision(table, relevant = "4"))/4,
       f1 = (F_meas(table, relevant = "1") + F_meas(table, relevant = "2") +
               F_meas(table, relevant = "3") + F_meas(table, relevant = "4"))/4
  )
}

# Funkcja pomocnicza: Box-Cox dla danych z wartościami ujemnymi
boxcox_signed <- function(x, eps = 1) {
  x = as.numeric(x)
  minimum = min(x)
  if (minimum < 0) {
    transformed = x - minimum + eps
  }
  transformed = BoxCox(transformed, lambda = 0)
  transformed = (transformed - mean(transformed))/sd(transformed) # przesunięcie o stałą -> transformacja B-C -> normalizacja szeregu
  return(transformed)
}

# Funkcja do zastosowania transformacji na całym zbiorze danych
apply_transformation <- function(data, method = c("sma", "boxcox"), sma.order=5) {
  data_trans <- data
  if (method == "sma") {
    data_trans = apply(data[, -1], MARGIN = 1, function(x) ma(x, order=sma.order))
    data_trans = t(na.omit(data_trans))
  } else if (method == "boxcox") {
    for (i in 1:nrow(data)) {
      series <- as.numeric(data[i, -1])
      data_trans[i, -1] <- boxcox_signed(series)
    }
    data_trans = data_trans[, -1]
  }
  return(data_trans)
}

transformations.AGNES = function(data.full, distances, k) {
  n = length(distances)
  
  # none.df = data.frame(matrix(0, nrow = n, ncol = 2))
  # colnames(none.df) = c("agreement", "silhouette")
  # rownames(none.df) = distances
  # for (i in 1:n) {
  #   result = My.AGNES(data.full, distance = distances[i], k)
  #   none.df$agreement[i] = result$agreement
  #   none.df$silhouette[i] = result$silhouette
  # }
  
  data.sma.5 = apply_transformation(data.full, method = "sma", sma.order = 5)
  data.sma.5 = as.data.frame(data.sma.5)
  # print(class(data.sma.5))
  sma.5.df = data.frame(matrix(0, nrow = n, ncol = 2))
  colnames(sma.5.df) = c("agreement", "silhouette")
  rownames(sma.5.df) = distances
  for (i in 1:n) {
    result = My.AGNES(cbind(data.full[, 1], data.sma.5), distance = distances[i], k)
    sma.5.df$agreement[i] = result$agreement
    sma.5.df$silhouette[i] = result$silhouette
  }
  
  data.sma.3 = apply_transformation(data.full, method = "sma", sma.order = 3)
  sma.3.df = data.frame(matrix(0, nrow = n, ncol = 2))
  colnames(sma.3.df) = c("agreement", "silhouette")
  rownames(sma.3.df) = distances
  for (i in 1:n) {
    result = My.AGNES(cbind(data.full[, 1], data.sma.3), distance = distances[i], k)
    sma.3.df$agreement[i] = result$agreement
    sma.3.df$silhouette[i] = result$silhouette
  }
  
  data.BC = apply_transformation(data.full, method = "boxcox")
  BC.df = data.frame(matrix(0, nrow = n, ncol = 2))
  colnames(BC.df) = c("agreement", "silhouette")
  rownames(BC.df) = distances
  for (i in 1:n) {
    result = My.AGNES(cbind(data.full[, 1], data.BC), distance = distances[i], k)
    BC.df$agreement[i] = result$agreement
    BC.df$silhouette[i] = result$silhouette
  }
  
  return(list(SMA.3.DF = sma.3.df, SMA.5.DF = sma.5.df, BC.DF = BC.df))
}

# load("D:/Magisterka/wd/data.RData")
# load("D:/Magisterka/wd/car_data.RData")
# load("D:/Magisterka/wd/ecg_data.RData")
load('/Volumes/NO NAME/Magisterka/wd/ecg_data.RData')
load('/Volumes/NO NAME/Magisterka/wd/data.RData')
load('/Volumes/NO NAME/Magisterka/wd/car_data.RData')

distances = c("dtw", "euclidean", "ccor", "cid", "fourier")
distances = c("cid", "fourier")

transformations.computers = transformations.AGNES(data.full, distances, 2) # start 12:00
transformations.car = transformations.AGNES(car.full, distances, 4)
transformations.ecg = transformations.AGNES(ecg.full, distances, 2)

# setwd("C:/Users/marcin/Desktop/temp Rdata")
save(transformations.computers, transformations.car, transformations.ecg, 
     file = '/Volumes/NO NAME/Magisterka/wd/transformations/transf AGNES/transformations_AGNES.RData')
# save(transformations.computers, file = "C:/Users/marcin/Desktop/temp Rdata/transformations_comp.RData")
# save(transformations.car, file = "C:/Users/marcin/Desktop/temp Rdata/transformations_car.RData")

NONE.DF.comp = as.data.frame(cbind(c(0.53, 0.5, 0.54, 0.56, 0.5), c(0.53, 0.09, 0.83, 0.58, 0.09)))
rownames(NONE.DF.comp) = c("dtw", "euclidean", "ccor", "cid", "fourier")
colnames(NONE.DF.comp) = c("agreement", "silhouette")
transformations.computers = list(NONE.DF = NONE.DF.comp, 
                                 SMA.3.DF = transformations.computers$SMA.3.DF, 
                                 SMA.5.DF = transformations.computers$SMA.5.DF, 
                                 BC.DF = transformations.computers$BC.DF)

NONE.DF.car = as.data.frame(cbind(c(0.42, 0.39, 0.43, 0.44, 0.39), c(0.43, 0.32, 0.38, 0.4, 0.32)))
rownames(NONE.DF.car) = c("dtw", "euclidean", "ccor", "cid", "fourier")
colnames(NONE.DF.car) = c("agreement", "silhouette")
transformations.car = list(NONE.DF = NONE.DF.car, 
                           SMA.3.DF = transformations.car$SMA.3.DF, 
                           SMA.5.DF = transformations.car$SMA.5.DF, 
                           BC.DF = transformations.car$BC.DF)

# zapisać do rdata
setwd("D:/Magisterka/wd")
save(computers, car, ecg, file = "transformations.RData")

computers = cbind(transformations.computers$NONE.DF, transformations.computers$SMA.DF$accuracy, transformations.computers$BC.DF$accuracy)
colnames(computers) = c("Raw", "SMA(5)", "Box-Cox")

car = cbind(transformations.car$NONE.DF, transformations.car$SMA.DF$accuracy, transformations.car$BC.DF$accuracy)
colnames(car) = c("Raw", "SMA(5)", "Box-Cox")

ecg = cbind(transformations.ecg$NONE.DF, transformations.ecg$SMA.DF$accuracy, transformations.ecg$BC.DF$accuracy)
colnames(ecg) = c("Raw", "SMA(5)", "Box-Cox")


