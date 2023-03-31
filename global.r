library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(expm)
library(imputeTS)

#binds data columnwise and fills with NA
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(,n-nrow(x), ncol(x)))))  #ignore error!
}


get_data <- function(){
  end <- Sys.Date()
  l <- list(rep(NA,length(assets_list)))
  Stocks <- lapply(assets_list, getSymbols, auto.assign = FALSE)
  Stocks <- setNames(Stocks, asl)
  for (i in 1:length(assets_list)){###########################33
    r <- Stocks[[i]]
    r <- cbind(r,diff(r$`CHF=X.Close`))
    colnames(r) <- c("Open","High","Low","Close","Volume","Adjusted")
    l[[i]] <- na_locf(r)

  }
  return(l)
}




mvp <- function(wa){
  zeithorizont = 365*2
  var_m <- data.frame()
  for (i in 1:length(asl)){
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
