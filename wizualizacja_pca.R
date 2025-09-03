library(factoextra)
library(ggplot2)

load("D:/Magisterka/wd/data.RData")
load('D:/Magisterka/wd/car_data.RData')
load('D:/Magisterka/wd/ecg_data.RData')

pca.viz = function(data, palette, title) {
  labels <- as.factor(data[[1]])

  data.numeric <- data[, -1]

  pca.res <- prcomp(data.numeric, scale. = TRUE)

  fviz_pca_ind(pca.res,
               geom.ind = "point",
               col.ind = labels,
               palette = palette,
               addEllipses = TRUE,
               legend.title = "Klasa",
               title = title)
}

setwd("D:/Magisterka/wd")
save(pca.viz, file = "pca_viz_function.RData")

pca.viz(data.full, c("red", "blue"), "xx")
pca.viz(car.full, c("red", "blue", "darkgreen", "black"), "xx")
pca.viz(ecg.full, c("blue", "red"), "xx")
