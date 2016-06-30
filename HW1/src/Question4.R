alpha = 3
beta = 2 
# Density function
par(mfrow = c(2, 3))
curve((((beta^alpha)/gamma(alpha))*x^(alpha - 1)*exp(-beta*x)), 0, 20)
# Data generated from RGamma
a = rgamma(1000, shape = alpha,rate = beta)
# QQ Plot for a
qqnorm(a)
# Histogram
hist(a)
plot(density(a, kernel = c("gaussian")))
plot(density(a, kernel = c("rectangular")))
plot(density(a, kernel = c("cosine")))


