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

get_data <- function() {
  end <- Sys.Date()
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
  
  # #interpolate usd to everyday
  # times.init <-index(l[[7]])
  # data2 <-zoo(l[[7]],times.init)
  # data3 <-merge(data2, zoo(, seq(min(times.init), max(times.init), "day")))
  # l[[7]] <-na.approx(data3)
  # 
  # #change usd to chf
  # usd = c(0,0,1,1,1,1) # 1 if asset is usd
  # for (p in length(usd)){
  #   if (usd[p] ==1){
  #     usd_ts <- l[[p]]
  #     chf_ts <- usd_ts
  #       for (i in 1:length(usd_ts[,1])) {
  #         id <- index(usd_ts)[i]
  #         fx_rate <- as.numeric(l[[7]]$Close[id]);fx_rate
  #         chf_ts[id] <- usd_ts[id]*fx_rate
  #       }
  #     l[[p]] <- chf_ts
  #   }
  # }
  
  ren <- list(rep(NA, length(assets_list)))
  for (i in 1:length(assets_list)){
    r <- na.omit(Stocks[[i]][,4])
    b <- suppressWarnings(dailyReturn(Stocks[[i]],type='log'))
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
  y <- na_locf(y)#!!!!!wichtig
  mittel <<- t(y) %*% rep(1 / N, N) * 260
  Sigma = cov(y, y)
  MVP1 <<- solve(Sigma) %*% rep(1, N2)
  MVP <<- MVP1 / sum(MVP1)
  mvpreturn <<- t(MVP) %*% mittel
  mvpvola <<- sqrt(t(MVP) %*% (Sigma %*% MVP)) * sqrt(260)
  return(as.array((MVP)))
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
  return(as.array((TP)))
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
    )) # rf return yourmoney return(rf)
}