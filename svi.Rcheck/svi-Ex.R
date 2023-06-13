pkgname <- "svi"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "svi-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('svi')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("axel")
### * axel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: axel
### Title: Axel-Vogt Parameters
### Aliases: axel
### Keywords: svi, implied volatility

### ** Examples

## Not run
# axel()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("axel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("durrleman")
### * durrleman

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: durrleman
### Title: Durrleman Condition
### Aliases: durrleman
### Keywords: svi, implied volatility

### ** Examples

## Not run
# load("sp500")
# df <- opt$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# fit <- svifit(x, y)
# plot(x, y)
# lines(x, fit$yhat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("durrleman", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom")
### * geom

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom
### Title: Geometric Properties
### Aliases: geom
### Keywords: svi, implied volatility

### ** Examples

## Not run
# load("sp500")
# df <- opt$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# fit <- svifit(x, y)
# plot(x, y)
# lines(x, fit$yhat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rnd")
### * rnd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rnd
### Title: Risk-Neutral Density
### Aliases: rnd
### Keywords: svi, implied volatility

### ** Examples

## Not run
# axel()
# q <- rnd(x, par)
# plot(x, q)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rnd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("svi")
### * svi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: svi
### Title: Calculate SVI predicted values
### Aliases: svi
### Keywords: svi, implied volatility

### ** Examples

## Not run
# load("sp500")
# df <- opt$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# fit <- svifit(x, y)
# plot(x, y)
# lines(x, fit$yhat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("svi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("svifit")
### * svifit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: svifit
### Title: Fitting SVI volatilities
### Aliases: svifit
### Keywords: svi, implied volatility

### ** Examples

## Not run
# library(svi)
# data(sp500)
# df <- sp500$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# fit <- svifit(x, y)
# plot(x, y)
# lines(x, fit$yhat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("svifit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("svipar")
### * svipar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: svipar
### Title: Translating conic coefficients into SVI parameters, and vice
###   versa
### Aliases: svipar
### Keywords: svi, implied volatility

### ** Examples

## Not run
# TBD



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("svipar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
