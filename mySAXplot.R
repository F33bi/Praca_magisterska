library(TSclust)

mySAX.plot <- function (series, w, alpha, col.ser = rainbow(ncol(as.matrix(series))), 
                        legend.position="bottomright", legend.bg="azure", legnames, main) 
{
  x <- series
  stopifnot(is.ts(x) || is.mts(x))
  n = length(x)
  x <- as.matrix(x)
  
  n.orig <- nrow(x)
  n.adj <- floor(n.orig / w) * w
  if (n.adj < n.orig) {
    x <- x[1:n.adj, , drop=FALSE]
    warning("Szereg został przycięty do długości ", n.adj, " aby był podzielny przez w = ", w)
  }
  n <- n.adj
  
  PAAser <- apply(x, 2, function(ser) {
    ser <- (ser - mean(ser))/sd(ser)
    PAAser <- PAA(ser, w)
  })
  SAXser <- apply(PAAser, 2, function(ser) {
    convert.to.SAX.symbol(ser, alpha)
  })
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2), nrow = 1), c(1, 5))
  mai <- par("mai")
  mai1 <- mai
  mai1[4] <- 0
  mai2 <- mai
  mai2[2] <- 0
  r <- range(TSclust:::SAX.breakpoints.table(alpha)[c(-1, -(alpha + 1))], 
             PAAser)
  par(mai = mai1)
  b <- seq(min(r), max(r), 0.01)
  plot(dnorm(b), b, type = "l", xlab = "", ylab = "", ann = FALSE, 
       xaxt = "n", bty = "n", ylim = r)
  abline(h = TSclust:::SAX.breakpoints.table(alpha), col = rainbow(alpha), 
         lty = 2)
  n <- length(x)
  k <- 1
  linetype <- "b"
  if (n%%w == 0) {
    k <- n/w
    PAAser <- matrix(rep(PAAser, each = k), byrow = F, ncol = ncol(x))
    linetype <- "l"
  }
  else {
    warning("Series length is not multiple of w (number of frames), plotting the dimensionality reduced series")
  }
  par(mai = mai2)
  plot(PAAser[, 1], type = linetype, pch = TSclust:::to.char.representation(SAXser[, 
                                                                         1]), ylim = r, col = col.ser[1], xlab = "", yaxt = "n", 
       bty = "n", main = main)
  abline(h = TSclust:::SAX.breakpoints.table(alpha), col = rainbow(alpha), 
         lty = 2)
  if (linetype == "l") {
    points(seq(1, n, k) + k/2, PAAser[seq(1, n, k), 1], pch = TSclust:::to.char.representation(SAXser[, 
                                                                                            1]), col = col.ser[1])
  }
  if (ncol(x) > 1) {
    for (i in 2:ncol(x)) {
      lines(PAAser[, i], type = linetype, pch = TSclust:::to.char.representation(SAXser[, 
                                                                              i]), ylim = r, col = col.ser[i])
      points(seq(1, n, k) + k/2, PAAser[seq(1, n, k), i], 
             pch = TSclust:::to.char.representation(SAXser[, i]), col = col.ser[i])
    }
  }
  # legnames <- colnames(x)
  if (is.null(legnames)) {
    legnames = 1:ncol(x)
  }
  legend(x=legend.position, bg=legend.bg, pch = 16, legend = legnames, col = col.ser)
  par(def.par)
}

save(mySAX.plot, file = 'D:/Magisterka/wd/mySAX_plot.RData')

load("D:/Magisterka/wd/car_data.RData")
mySAX.plot(cbind(as.ts(as.numeric(car.train[1, -1])),
                 as.ts(as.numeric(car.train[2, -1]))),
           w = 10, alpha = 8, main = "Car - Wizualizacja SAX", legnames = c("1", "2"))

mySAX.plot(AirPassengers, w = 20, alpha = 6, main = "Wizualizacja SAX")
