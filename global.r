library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(expm)
library(imputeTS)

#binds data columnwise and fills with NA
cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(
      , n - nrow(x), ncol(x)
    ))))  #ignore error!
}

usdchf <- function(){
  usd_chf <- suppressWarnings(getSymbols(fx, src = "yahoo", auto.assign = FALSE)[,4])
  colnames(usd_chf) <- "usd_chf"
  usd_chf <<- usd_chf
}

get_data <- function() {
  l <- list(rep(NA, length(assets_list)))
  Stocks <- suppressWarnings(lapply(assets_list, getSymbols, auto.assign = FALSE))
  Stocks <- setNames(Stocks, asl)
  for (i in 1:length(assets_list)) {
    r <- Stocks[[i]]
    colnames(r) <-
      c("Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Adjusted")
    l[[i]] <- (r)
    attributes(l[[i]])$na.action <- NULL
  }
  
  #remove weekend from bitcoin
  x <- l[[4]]
  l[[4]] <- NA
  x <-x[.indexwday(x) %in% 1:5]
  l[[4]] <- x
  
  
  #change usd to chf
  usd = c(0,0,1,1,1,1) # 1 if asset is usd
  for (p in 1:length(usd)){
    q <- usd[p]
    if(q>=1){
      usd_ts <- l[[p]]
      chf_ts <- na.omit(merge(usd_ts,usd_chf, all=F))
      attributes(chf_ts)$na.action <- NULL
      for (i in 1:length(colnames(usd_ts))) {
        chf_ts[,i] <- chf_ts[,i]*chf_ts[,7]
      }
      chf_ts <- chf_ts[,-7]
      l[[p]] <- NA
      l[[p]] <- as.xts(na.omit(chf_ts))
    }
  }
  
  
  
  ren <- list(rep(NA, length(assets_list)))
  for (i in 1:length(assets_list)){
    r <- na.omit(Stocks[[i]][,4])
    b <- suppressWarnings(dailyReturn(Stocks[[i]],type='arithmetic'))
    r <- as.data.frame(r)
    r$rendite <- b
    r <- as.xts(r)
    colnames(r) <-
      c("Close",paste0("r.", asl[i]))
    ren[[i]] <- na.omit(r)
    attributes(ren[[i]])$na.action <- NULL
  }
  data <- list(l,ren)
  return(data)
}

calculate_alpha <- function(weights, returns_list) {
  
  # Calculate portfolio return
  portfolio_returns <- Reduce(`+`, lapply(seq_along(weights), function(i) weights[i] * returns_list[[i]]))
  
  # Fit linear regression model
  model <- lm(portfolio_returns ~ returns_list[[7]])
  
  # Extract alpha from model coefficients
  alpha <- coef(model)[1]
  
  return(alpha)
}



mvp <- function(y) {
  ret.mat <- as.matrix(na.omit(y))
  exp.rets<-colMeans(exp(ret.mat)) - 1;exp.rets
  COV <-cov(ret.mat)
  MVP_v <-globalMin.portfolio(exp.rets, COV)
  MVP <<- MVP_v$weights
  mvpreturn <<- MVP_v$er
  mvpvola <<- MVP_v$sd
  return(as.array(MVP))
}


tp <- function(y,shortpara=F) {
  
  ret.mat <- as.matrix(na.omit(y))
  exp.rets<-colMeans(exp(ret.mat)) - 1;exp.rets
  COV <-cov(ret.mat)
  risk.free <- 0.00001
  
  TP_v <- tangency.portfolio(exp.rets,COV,risk.free=risk.free,shorts = shortpara)
  TP <<- TP_v$weights
  tpreturn <<- TP_v$er
  tpvola <<- TP_v$sd
  Sigma_t <<- cov(y, y)
  return(as.array(TP))
}


get_rf <- function() {
  URL <-
    "https://www.yourmoney.ch/ym/details/4961368%2C1526%2C1#Tab0"
  pagecode <- xml2::read_html(x = URL)
  pagecode <- rvest::html_nodes(pagecode, css = ".detailPrice")
  pagecode <- rvest::html_text(pagecode)
  pagecode_clean <- gsub(" ", "", pagecode, fixed = TRUE)
  rf <-
    as.numeric(substr(
      x = pagecode_clean[1],
      start = 1,
      stop = nchar(pagecode_clean[1]) - 1
    ))/100 # rf return yourmoney return(rf)
}

portfolio_w_F <- function() {
  portfolio_w <- c()
  for (i in 1:length(portfolio_s)) {
    portfolio_w[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
  }
  portfolio_w <<- round((portfolio_w), 1)
}

dat_mvp_F <- function() {
  a <- data.frame()
  for (i in 1:length(portfolio_s)) {
    if (portfolio_s[i] > 0)
      a <- cbind.fill(a, ren[[i]][, 2])
  }
  dat_v <- mvp(a)
  
  dat_mvp <<- data.frame(Asset = rownames(dat_v),
                         Gewicht = c(dat_v))
}


dat_tp_F <- function(shortpara=F) {
  a <- data.frame()
  for (i in 1:length(portfolio_s)) {
    if (portfolio_s[i] > 0)
      a <- cbind.fill(a, ren[[i]][, 2])
  }
  dat_v <- tp(a,shortpara)
  dat_tp <<- data.frame(Asset = rownames(dat_v),
                        Gewicht = c(dat_v))
}

dat_mvp_rec_F <- function() {
  dat_mvp_rec <- dat_mvp
  dat_mvp_rec$Gewicht <- round(dat_mvp$Gewicht, 2)
  as.data.frame(dat_mvp_rec)
  g <- c(portfolio_w)
  g <- g[g != 0]
  lp <- c(1:length(g))
  sg <- sum(g)
  abssum<- sum(abs(dat_mvp[lp, 2]))
  g <- sg/abssum*dat_mvp[lp, 2]
  dat_mvp_rec$Investiert <- (g)
  pa <- portfolio_w
  for (i in 1:length(asl)) {
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
  }
  pa <- pa[pa != 0]
  dat_mvp_rec$Anzahl <- round(g / pa,1)
  n = length(dat_mvp_rec$Anzahl)
  h = c(rep(NA, n))
  for (i in 1:n) {
    if (portfolio_s[i] - dat_mvp_rec$Anzahl[i] < 0)
      h[i] <- "Kaufen"
    else if (portfolio_s[i] - dat_mvp_rec$Anzahl[i] > 0)
      h[i] <- "Verkaufen"
    else if (portfolio_s[i] - dat_mvp_rec$Anzahl[i] == 0)
      h[i] <- "Halten"
  }
  dat_mvp_rec$Handlung <- h
  dat_mvp_rec <-
    dat_mvp_rec[order(abs(dat_mvp_rec$Investiert), decreasing = T), ]
  dat_mvp_rec <<- dat_mvp_rec
}


dat_tp_rec_F <- function() {
  dat_tp_rec <- dat_tp
  dat_tp_rec$Gewicht <- round(dat_tp$Gewicht, 2)
  as.data.frame(dat_tp_rec)
  g <- c(portfolio_w)
  g <- g[g != 0]
  lp <- c(1:length(g))
  sg <- sum(g)
  abssum<- sum(abs(dat_tp[lp, 2]))
  g <- sg/abssum*dat_tp[lp, 2]
  dat_tp_rec$Investiert <- (g)
  pa <- portfolio_w
  for (i in 1:length(asl)) {
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
  }
  pa <- pa[pa != 0]
  dat_tp_rec$Anzahl <- round(g / pa,1)
  n = length(dat_tp_rec$Anzahl)
  h = c(rep(NA, n))
  for (i in 1:n) {
    if (portfolio_s[i] - dat_tp_rec$Anzahl[i] < 0)
      h[i] <- "Kaufen"
    else if (portfolio_s[i] - dat_tp_rec$Anzahl[i] > 0)
      h[i] <- "Verkaufen"
    else if (portfolio_s[i] - dat_tp_rec$Anzahl[i] == 0)
      h[i] <- "Halten"
  }
  dat_tp_rec$Handlung <- h
  dat_tp_rec <-
    dat_tp_rec[order(abs(dat_tp_rec$Investiert), decreasing = T), ]
  dat_tp_rec <<- dat_tp_rec
}



