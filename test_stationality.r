stetige_rendite <- function(zeitreihe) {
  t <- length(zeitreihe)
  ergebnis <- rep(NA, t)
  ergebnis <- numeric(t - 1)
  for (i in 2:t) {
    ergebnis[i - 1] <- log(zeitreihe[i]) - log(zeitreihe[i - 1])
  }
  return(ergebnis)
}

a <- stetige_rendite(dat_asset[[1]])
diff(dat_asset[[1]])


kurse <- as.matrix(dat_asset[[1]])
t <- length(kurse)
differenzen <- kurse[2:t] - kurse[1:(t - 1)]
rendite <- differenzen / kurse[1:(t - 1)]
return(rendite)


attributes(dat_asset[[1]])$na.action <- NULL
a <-as.xts(dat_asset[[1]])
a$Close
a <- dat_asset[[1]]
a
a
class(dat_asset[[1]]$rendite.SMI)
