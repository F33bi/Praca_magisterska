library(feasts)

data = ts(cbind(cumsum(rnorm(100)), rnorm(100, 1, 1)))
plot(data)


feat_spectral(data)

data = read.delim("C:/Users/marcin/Downloads/UCRArchive_2018/UCRArchive_2018/FiftyWords/FiftyWords_TRAIN.tsv", header = F)
feat_acf(data)

a = diss(data, "PACF")
b = as.matrix(a)

plot(b)
