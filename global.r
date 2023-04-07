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
  for (i in 1:length(assets_list)){
    r <- Stocks[[i]]
    r$diffs <- (diff(dat_asset[[i]]$Close))
    colnames(r) <- c("Open","High","Low","Close","Volume","Adjusted",paste0("diff.", asl[i]))
    l[[i]] <- na.omit(r)
    
  }
  return(l)
}


mvp<- function(y){
  N=dim(y)[1]
  N2=dim(y)[2]
  y <- na_locf(y)
  mittel=t(y)%*%rep(1/N,N)*260
  Sigma=cov(y,y)
  MVP1=solve(Sigma)%*%rep(1,N2)
  MVP=MVP1/sum(MVP1)
  mvpreturn=t(MVP)%*%mittel
  mvpvola=sqrt(t(MVP)%*%(Sigma%*%MVP))*sqrt(260)
  return(as.array((MVP)))
  } 


tp<-function(y){N=dim(y)[1]
  excess=t(y)%*%rep(1/N,N)*260-riskfree
  Sigma=cov(y,y);
  TP1=solve(Sigma)%*%excess
  TP=TP1/sum(TP1); TP=TP[,1]
  tpreturn=t(TP)%*%(excess+riskfree)
  tpvola=sqrt(t(TP)%*%(Sigma%*%TP))*sqrt(260)
  c(TP, tpreturn, tpvola)
  } 







