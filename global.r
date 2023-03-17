library(quantmod)
library(devtools)
library(stringr)
library(tradingeconomics)
login('Your_Key:Your_Secret')
install_github("tradingeconomics/tradingeconomics/R/tradingeconomics")


assets_list <- c("GC=F","BTC-USD","^GSPC","UTEN","CHF=X")

get_data <- function(x){
  a <- getSymbols(x,src='yahoo',auto.assign=FALSE)
  a <- na.approx(a)
  return(a)
}

n <- 1000 #number of days as history
l <- matrix(nrow=n,ncol=length(assets_list))
nrow(l)
for (i in 1:length(assets_list)){
  r <- get_data(assets_list[i])
  r <- rev(r)
  r <- r[,4]
  r <- r[1:n]
  l[,i] <- r
  
}
l


