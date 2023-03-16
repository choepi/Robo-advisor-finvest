library(quantmod)
library(zoo)
library(xts)
library(dplyr)


pull <- function(x){
  a <- getSymbols(x,src='yahoo',auto.assign=FALSE)
  a <- fortify.zoo(a)
  return(a)
}

get_data <- function(n){
  assets_list <- c("^SSMI","0VPY.L","GC=F","BTC-USD","^GSPC","^TNX","CHF=X")
  asl <- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND","USDCHF")
  st <- Sys.Date()
  en <- st-n
  #en <- as.character(en)
  #n = number of days as history
  for (i in 1:length(assets_list)){
    r <- pull(assets_list[i])
    # transpose of dataframe
    transpose <- t(r)
    transpose <- as.data.frame(transpose)
    rev_data_frame <- rev(transpose)
    rev_data_frame <- t(rev_data_frame)
    r <- as.data.frame(rev_data_frame)
    #r[,1] <- as.Date(r[,1])
    e <- nrow(r %>% filter(r[,1] > en))
    s <- r[1:e,c(1,5)]
    assign(asl[i], s, envir = .GlobalEnv)
  }
}

get_data(1000)



