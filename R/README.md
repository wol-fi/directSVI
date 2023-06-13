# Installation

devtools::install_github("wol-fi/directSVI/R")

# Example

library(svi)
data(sp500)
df <- sp500$`tau = 7 days`
x <- df$k
y <- df$ivol^2      # Note: y = implied variance (squared Black-Scholes imp.vol.)
fit <- svifit(x, y)
plot(x, y)
lines(x, fit$yhat)
