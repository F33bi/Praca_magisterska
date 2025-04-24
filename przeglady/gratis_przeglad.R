library(gratis)
library(forecast)
mod1 = arima_model(frequency = 4, p = 1, d = 0, q = 1, P = 1, D = 1, Q = 1, constant = 1)
simulate(mod1, 36) %>% plot()

mod2 = ets_model(frequency = 4, error = "A", trend = "A", seasonal = "A", damped = F)
simulate(mod2, 36) %>% plot()

mod3 = mar_model(k = 2, p = c(1, 0), d = c(0, 1), P = c(1, 1), constants = c(0, 2), weights = c(0.9, 0.1))
ts(simulate(mod3, 36), frequency = 4) %>% plot()

pi_coefficients(ar = 1, d = 1, ma = 1, sar = 1, D = 2, sma = 1, m = 1)

rmixnorm(1000, means = c(0, 5), sigmas = c(1, 1), weights = c(0.5, 0.5)) %>% hist(breaks = 20)
# rmixnorm_ts(1000, means.ar.par.list = list(c(0, 1), c(1, 0)), sigmas.list = sigmas.list, weights = c(0.5, 0.5)) %>% plot()


