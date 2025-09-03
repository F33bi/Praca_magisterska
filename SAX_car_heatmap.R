library(TSdist)
library(caret)

alpha_vals <- c(2, 4, 8, 12, 16, 20, 24)
w_vals <- c(2, 4, 10, 20, 30, 45, 60)

SAX_heatmap_function = function(alpha_vals, w_vals, data.train, data.test) {
  
  results <- data.frame(w = numeric(0),
                        alpha = numeric(0),
                        accuracy = numeric(0))
  
  for (alpha in alpha_vals) {
    for (w in w_vals) {
      print(c(alpha, w))
      
      # res <- My.KMedoids(data.full[, -1], data.full[, 1], k=2, 
      #                    "mindist.sax", w=w, alpha=alpha)
      
      res <- My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1],
                      "mindist.sax", w=w, alpha=alpha)
      
      results <- rbind(results, data.frame(w = w, alpha = alpha, accuracy = res$accuracy))
    }
  }
  
  w_vals_sorted <- sort(unique(results$w))
  alpha_vals_sorted <- sort(unique(results$alpha))
  
  accuracy_matrix <- matrix(NA, nrow = length(alpha_vals_sorted), ncol = length(w_vals_sorted))
  
  for (i in 1:length(alpha_vals_sorted)) {
    for (j in 1:length(w_vals_sorted)) {
      accuracy_matrix[i, j] <- results$accuracy[results$w == w_vals_sorted[j] & results$alpha == alpha_vals_sorted[i]]
    }
  }
  
  filled.contour(x = w_vals_sorted,
                 y = alpha_vals_sorted,
                 z = accuracy_matrix,
                 color.palette = terrain.colors,
                 xlab = "w",
                 ylab = "alpha",
                 main = "Dokładność metody SAX w zależności od parametrów")
}

load('D:/Magisterka/wd/ecg_data.RData')
SAX_heatmap_function(alpha_vals, w_vals, ecg.train, ecg.test)

setwd("D:/Magisterka/wd")
save(car.train, car.test,
     alpha_vals, w_vals,
     My.oneNN,
     SAX_heatmap_function, file = "car_SAX_heatmap.RData")


