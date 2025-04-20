library(ggplot2)
library(forecast)

data.train = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TRAIN.txt')
data.test = read.table('/Volumes/NO NAME/Magisterka/Dane/Computers/Computers_TEST.txt')
data.full = rbind(data.train, data.test)

godziny <- format(seq.POSIXt(from = as.POSIXct("2025-01-01 00:00"),
                             by = "2 min", length.out = 720), 
                  format = "%H:%M")

# losowe szeregi
set.seed(123)
random.train = data.train[sample(1:250, 1), -1]
random.test = data.test[sample(1:250, 1), -1]
random.train = as.ts(as.numeric(random.train))
random.test = as.ts(as.numeric(random.test))

plot(as.numeric(random.train), type = "l", xaxt = "n", xlab = "Czas", ylab = "Wartość", 
     col = terrain.colors(3)[1], main = "Wizualizacja losowych szeregów")
lines(as.numeric(random.test), type = "l", col = terrain.colors(3)[2])
axis(1, at = seq(1, 720, by = 30), labels = godziny[seq(1, 720, by = 30)])
legend("topleft",
       legend = c("zbiór uczący", "zbiór testowy"),
       col = terrain.colors(3)[1:2], lty = 1)

# średnie szeregi
avg.train = colMeans(data.train[, -1])
avg.test = colMeans(data.test[, -1])
avg.train = as.ts(as.numeric(avg.train))
avg.test = as.ts(as.numeric(avg.test))

plot(as.numeric(avg.train), type = "l", xaxt = "n", xlab = "Czas", ylab = "Wartość", 
     col = terrain.colors(3)[1], main = "Wizualizacja średnich szeregów")
lines(as.numeric(avg.test), type = "l", col = terrain.colors(3)[2])
axis(1, at = seq(1, 720, by = 30), labels = godziny[seq(1, 720, by = 30)])
legend("topleft",
       legend = c("zbiór uczący", "zbiór testowy"),
       col = terrain.colors(3)[1:2], lty = 1)






