load("D:/Magisterka/wd/1NN.RData")

df = rbind(ACF, DTW, Euclidean, SAX, CCor, CID, Fourier)
df = df[, -1]

df
