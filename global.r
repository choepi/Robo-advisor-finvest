library(quantmod)

pull <- function(x){
  a <- getSymbols(x,src='yahoo',auto.assign=FALSE)
  a <- na.approx(a)
  return(a)
}
get_data <- function(n){
  assets_list <- c("GC=F","BTC-USD","^GSPC","UTEN","CHF=X")
  #n = number of days as history
  l <- matrix(nrow=n,ncol=length(assets_list))
  nrow(l)
  for (i in 1:length(assets_list)){
    r <- get_data(assets_list[i])
    r <- rev(r)
    r <- r[,4]
    r <- r[1:n]
    l[,i] <- r
  }
  return(l)
}

get_data(10)
l
