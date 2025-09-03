library(cluster)
library(e1071)

# load("D:/Magisterka/wd/ecg_data.RData")
# load('/Volumes/NO NAME/Magisterka/wd/data.RData')
load('/Volumes/NO NAME/Magisterka/wd/car_data.RData')

SAX = My.AGNES(car.full, k = 4, "mindist.sax", w = 2)
Euclidean = My.AGNES(car.full, k = 4, "euclidean")
CCor = My.AGNES(car.full, k = 4, "ccor")
DTW = My.AGNES(car.full, k = 4, "dtw")
ACF = My.AGNES(car.full, k = 4, "acf", lag.max = 5)
CID = My.AGNES(car.full, k = 4, "cid")
Fourier = My.AGNES(car.full, k = 4, "fourier")

df.car.AGNES = rbind(ACF, DTW, Euclidean, SAX, CCor, CID, Fourier)
df.car.AGNES = df.car.AGNES[, -1]
df.car.AGNES[2] = as.numeric(df.car.AGNES[2])*60

colnames(df.car.AGNES)[1] = "time (sec)"

save(df.car.AGNES, file = "/Volumes/NO NAME/Magisterka/wd/AGNES/df_AGNES_car.RData")

