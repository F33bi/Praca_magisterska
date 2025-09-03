library(TSclust)

par(mfrow = c(1, 2))
SAX.plot(as.ts(as.numeric(car.test[1, -1])), w = 10, alpha = 8)
lines(x=seq(0, 10, length.out = 577), y=as.numeric(car.test[1, -1])/2, type = "l")

plot(as.numeric(car.test[1, -1])/2)

