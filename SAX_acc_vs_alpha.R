load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')

alpha.vec = seq(2, 24, by = 2)
time.vec = NULL
acc.vec = NULL

for (alpha in alpha.vec) {
  print(alpha)
  set.seed(123)
  a = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               distance = "mindist.sax", alpha = alpha, w = 9) # najlepsze w
  time.vec = append(time.vec, a$time)
  acc.vec = append(acc.vec, a$accuracy)
}

plot(alpha.vec, acc.vec, type = "l", main = "Accuracy vs alpha in SAX method", 
     xlab = "alpha", ylab = "Accuracy", lty = "dotted", col = "darkgrey",
     ylim = c(0.49, 0.57), xlim = c(1.5, 24.5), xaxt = "n")
axis(1, at = alpha.vec)
points(alpha.vec, acc.vec, pch = 16, col = "black")
text(alpha.vec, acc.vec + rep(c(-0.006, 0.006), 6), 
     labels = paste0(round(time.vec, 2), " s"), 
     cex = 0.65, col = "blue")
