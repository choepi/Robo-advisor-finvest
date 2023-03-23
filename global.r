library(quantmod)
library(zoo)
library(xts)
library(dplyr)


get_data <- function(n){
  assets_list <- c("^SSMI","CSBGC0.SW","GC=F","BTC-USD","^GSPC","^TNX","CHF=X")
  asl <- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND","USDCHF")
  end <- Sys.Date()
  start <- as.Date(end-n)
  l <- list(rep(NA,length(assets_list)))
  Stocks <- lapply(assets_list, getSymbols, auto.assign = FALSE)
  Stocks <- setNames(Stocks, asl)
  for (i in 1:length(assets_list)){
    r <- Stocks[[i]]
    #r <- window(r, start = end, end=start)
    #r <- r[,4]
    colnames(r) <- c("Open","High","Low","Close","Volume","Adjusted")
    l[[i]] <- r
  }
  return(l)
}


