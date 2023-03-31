stetige_rendite <- function(zeitreihe) {
  T <- length(zeitreihe)
  ergebnis <- rep(NA, T)
  ergebnis <- numeric(T - 1)
  for (i in 2:T) {
    ergebnis[i - 1] <- log(zeitreihe[i]) - log(zeitreihe[i - 1])
  }
  return(ergebnis)
}

stetige_rendite(dat_asset[[1]]$Close)
diff(dat_asset[[1]])
