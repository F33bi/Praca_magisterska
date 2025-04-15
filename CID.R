library(TSclust)

data = read.table('/Volumes/NO NAME/Magisterka/Computers/Computers_TRAIN.txt')

ncol(data)
nrow(data)

diss.matrix.CID = matrix(nrow = 250, ncol = 250, data = 0)

start = Sys.time()
for (first_series in 1:250) {
  for (second_series in first_series:250) {
    diss.matrix.CID[first_series, second_series] = 
      diss.CID(as.numeric(data[first_series, 2:721]), as.numeric(data[second_series, 2:721]))
  }
}
Sys.time() - start
time.in.min = 2.370282

setwd('/Volumes/NO NAME/Magisterka/wd')
save(diss.matrix.CID, time.in.min, file = "CID.RData")
