library(forecast)

boxcox_signed <- function(x, eps = 0.001) {
  x = as.numeric(x)
  minimum = min(x)
  if (minimum < 0) {
    transformed = x - minimum + eps
  }
  transformed = BoxCox(transformed, lambda = 0)
  transformed = (transformed - mean(transformed))/sd(transformed) # przesunięcie o stałą -> transformacja B-C -> normalizacja szeregu
  return(transformed)
}

transformation.plots = function(series, data.name) {
  series = as.numeric(series)
  par(mfrow = c(2, 2))
  plot(series, type = "l", xlab = "Time", ylab = NA, main = paste0(data.name, " Raw"))
  plot(ma(series, order=3), col = "red", ylab = NULL, main = paste0(data.name, " SMA3"))
  plot(ma(series, order=5), col = "blue", ylab = NULL, main = paste0(data.name, " SMA5"))
  plot(boxcox_signed(series), col = "green", type = "l", xlab = "Time", ylab = NA, 
       main = paste0(data.name, " Box-Cox"))
}

load("D:/Magisterka/wd/data.RData")
transformation.plots(data.test[1, -1], "Computers")

load("D:/Magisterka/wd/car_data.RData")
transformation.plots(car.test[1, -1], "Car")

load("D:/Magisterka/wd/ecg_data.RData")
transformation.plots(ecg.test[1, -1], "ECG200")
