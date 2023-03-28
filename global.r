library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(expm)
library(imputeTS)

get_data <- function(){
<<<<<<< HEAD
  assets_list <- c("^SSMI","CSBGC0.SW","GC=F","BTC-USD","^GSPC","^TNX","CHF=X")
  asl <- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND","USDCHF")
=======
>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c
  end <- Sys.Date()
  #start <- as.Date(end-n)
  l <- list(rep(NA,length(assets_list)))
  Stocks <- lapply(assets_list, getSymbols, auto.assign = FALSE)
  Stocks <- setNames(Stocks, asl)
  for (i in 1:length(assets_list)){
    r <- Stocks[[i]]
    #r <- window(r, start = end, end=start)
    #r <- r[,4]
    colnames(r) <- c("Open","High","Low","Close","Volume","Adjusted")
    l[[i]] <- na.omit(r)
  }
  return(l)
}

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

<<<<<<< HEAD

=======
#na.locf?
>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c

mvp <- function(wa){
  zeithorizont = 365*2
  var_m <- data.frame()
<<<<<<< HEAD
  asl <- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND","USDCHF")
  for (i in 1:7){
=======
  for (i in 1:length(asl)){
>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c
    x <- (dat_asset[[i]])$Close
    x <- x[1:zeithorizont]
    var_m <- cbind.fill(var_m,x$Close)
  }
  
  colnames(var_m) <- asl
  var_m <- na_interpolation(var_m, option = "linear")
  
  #m <- matrix(diag(as.vector(var_m)), ncol = sum(wa));m#unkorrelierte assets
  m <- cov(var_m)
  #m <- rbind(c(0.04,0.01,0),c(0.01,0.05,0),c(0,0,0.02))
  m <- round(m,3) 
  m_inv <- round(solve(m),3)
  
  #korreliert?
  #m <- cor(m)
  #diag(m) <- var_m
  
  s <- (1/(t(rep(1,ncol(m)))%*%m%*%rep(1,ncol(m))))%*%rep(1,ncol(m))%*%m_inv
  w <- round(s/sum(s),3);w
  return(w)
}


