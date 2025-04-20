library(TSdist)

load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')

w.vec = seq.int(1, 53, by = 4)
time.vec = NULL
acc.vec = NULL

for (w in w.vec) {
  print(w)
  set.seed(123)
  a = My.oneNN(data.train[, -1], data.train[, 1], data.test[, -1], data.test[, 1], 
               distance = "mindist.sax", w = w)
  time.vec = append(time.vec, a$time)
  acc.vec = append(acc.vec, a$accuracy)
}

plot(w.vec, acc.vec, type = "l", main = "Accuracy vs w in SAX method", 
     xlab = "w", ylab = "Accuracy", lty = "dotted", col = "darkgrey", 
     xlim = c(0.75, 53.25), ylim = c(0.43, 0.57), xaxt = "n")
axis(1, at = w.vec)
points(w.vec, acc.vec, pch = 16, col = "black")
text(w.vec, acc.vec + rep(c(-0.015, 0.015), 7), 
     labels = paste0(round(time.vec, 2), " s"), 
     cex = 0.65, col = "blue")
