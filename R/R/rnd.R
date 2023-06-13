# file:///C:/Users/WSchadner/Downloads/1-RobustCalibrationForSVIModelArbitrageFree.pdf
rnd <- function(x, par){
  g <- durrleman(x, par)
  w <- svi(x, par)
  vol <- sqrt(w)
  dm <- -x/vol - vol/2
  p <- g/sqrt(2*pi*w)*exp(-0.5*dm^2)
  return(p)
}
