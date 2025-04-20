load('/Volumes/NO NAME/Magisterka/wd/1NN.RData')

df = rbind(ACF, DTW, Euclidean, SAX, CCor, CID)
df = df[, -1]

df
