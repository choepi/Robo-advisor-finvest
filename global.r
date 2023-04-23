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
  usd_chf <- getSymbols(fx, src = "yahoo", auto.assign = FALSE)[,4]
  colnames(usd_chf) <- "Close"
  usd_chf <<- usd_chf**-1
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
  x <-x[.indexwday(x) %in% 1:5]
  replace(l, 4, x)
  l[[4]] <- x
  
  
  # #change usd to chf
  # usd = c(0,0,1,1,1,1) # 1 if asset is usd
  # for (p in length(usd)){
  #   if (usd[p] ==1){
  #     usd_ts <- as.data.frame(l[[p]])
  #     chf_ts <- as.data.frame(usd_ts)
  #       for (i in 1:length(usd_ts[,1])) {
  #         id <- index(usd_ts)[i]
  #         fx_rate <- as.numeric(usd_chf[id,]);fx_rate
  #         chf_ts[id,] <- usd_ts[id,]*fx_rate
  #       }
  #     l[[p]] <- na.omit(chf_ts)
  #   }
  # }
  
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
  N = dim(y)[1]
  N2 = dim(y)[2]
  y <- na_locf(y)#!!!!!wichtig1
  mittel <<- t(y) %*% rep(1 / N, N) * 260
  Sigma = cov(y, y)
  MVP1 <<- solve(Sigma) %*% rep(1, N2)
  MVP <<- MVP1 / sum(MVP1)
  mvpreturn <<- t(MVP) %*% mittel
  mvpvola <<- sqrt(t(MVP) %*% (Sigma %*% MVP)) * sqrt(260)
  return(as.array(MVP))
}


tp <- function(y) {
  N = dim(y)[1]
  y <- na_locf(y)#!!!!!wichtig
  excess = t(y) %*% rep(1 / N, N) * 260 - riskfree
  Sigma_t <<- cov(y, y)
  TP1 = solve(Sigma_t) %*% excess
  TP = TP1 / sum(TP1)
  TP <<- TP[, 1]
  tpreturn <<- t(TP) %*% (excess + riskfree)
  tpvola <<- sqrt(t(TP) %*% (Sigma_t %*% TP)) * sqrt(260)
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
  portfolio_w <<- round((portfolio_w), 0)
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


dat_tp_F <- function() {
  a <- data.frame()
  for (i in 1:length(portfolio_s)) {
    if (portfolio_s[i] > 0)
      a <- cbind.fill(a, ren[[i]][, 2])
  }
  dat_v <- tp(a)
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
  g <- sum(g)
  g <- g * dat_mvp[lp, 2]
  dat_mvp_rec$Investiert <- (g)
  pa <- portfolio_w
  for (i in 1:length(asl)) {
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
  }
  pa <- pa[pa != 0]
  dat_mvp_rec$Anzahl <- round(g / pa)
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
  g <- sum(g)
  g <- g * dat_tp[lp, 2]
  dat_tp_rec$Investiert <- (g)
  pa <- portfolio_w
  for (i in 1:length(asl)) {
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
  }
  pa <- pa[pa != 0]
  dat_tp_rec$Anzahl <- round(g / pa)
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

