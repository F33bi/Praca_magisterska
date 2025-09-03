load("D:/Magisterka/wd/car_data.RData")


SAX.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
               "mindist.sax", w = 30)
Euclidean.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
                     "euclidean")
CCor.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
                "ccor")
DTW.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
               "dtw")
ACF.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
               "acf", lag.max = 5)
CID.car = My.oneNN(car.train[, -1], car.train[, 1], car.test[, -1], car.test[, 1], 
               "cid")

df.car = rbind(ACF.car, DTW.car, Euclidean.car, SAX.car, CCor.car, CID.car)
df.car = df.car[, -1]
rownames(df.car) = c("ACF", "DTW", "Euclidean", "SAX", "CCor", "CID")

df.car

# setwd("D:/Magisterka/wd")
# save(car.train, car.test, car.full, file = "car_data.RData")




