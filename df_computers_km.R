library(TSdist)
library(caret)

# load("D:/Magisterka/wd/KMedoids_results.RData")
# load("D:/Magisterka/wd/My_KMedoids_function.RData")
# load("D:/Magisterka/wd/data.RData")
load("D:/Magisterka/wd/ecg_data.RData")

SAX = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                  "mindist.sax", w = 2)
Euclidean = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                        "euclidean")
CCor = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                   "ccor")
DTW = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                  "dtw")
ACF = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                  "acf", lag.max = 5)
CID = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                  "cid")
Fourier = My.KMedoids(ecg.full[, -1], ecg.full[, 1], k = 2, 
                      "fourier")

df.ecg.km = rbind(ACF, DTW, Euclidean, SAX, CCor, CID, Fourier)
df.ecg.km = df.ecg.km[, -1]
df.ecg.km[2] = as.numeric(df.ecg.km[2])*60

colnames(df.ecg.km)[1] = "time (sec)"

setwd("D:/Magisterka/wd")
save(df.ecg.km, file = "df_KMed_ecg.RData")




