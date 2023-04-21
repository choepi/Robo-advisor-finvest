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

rendite_calc <- function(a) {
  kurse <- as.data.frame(a)
  rendite <- kurse[, -1]
  t <- length(kurse[, 1])
  differenzen <- kurse[2:t, ] - kurse[1:(t - 1), ]
  rendite <- cbind.fill(rendite, differenzen / kurse[1:(t - 1), ])
  return(as.xts(rendite))
}

get_data <- function() {
  end <- Sys.Date()
  l <- list(rep(NA, length(assets_list)))
  Stocks <- lapply(assets_list, getSymbols, auto.assign = FALSE)
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
    l[[i]] <- na.omit(r)
    attributes(l[[i]])$na.action <- NULL
  }
  ren <- list(rep(NA, length(assets_list)))
  for (i in 1:length(assets_list)){
    r <- Stocks[[i]][,4]
    b <- rendite_calc(r)
    b <- cbind.fill(r, b)[, 2]
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


mvp <- function(y) {
  N = dim(y)[1]
  N2 = dim(y)[2]
  y <- na_locf(y)#!!!!!wichtig
  mittel = t(y) %*% rep(1 / N, N) * 260
  Sigma = cov(y, y)
  MVP1 = solve(Sigma) %*% rep(1, N2)
  MVP = MVP1 / sum(MVP1)
  mvpreturn <<- t(MVP) %*% mittel
  mvpvola <<- sqrt(t(MVP) %*% (Sigma %*% MVP)) * sqrt(260)
  return(as.array((MVP)))
}


tp <- function(y) {
  N = dim(y)[1]
  y <- na_locf(y)#!!!!!wichtig
  excess = t(y) %*% rep(1 / N, N) * 260 - riskfree
  Sigma = cov(y, y)
  TP1 = solve(Sigma) %*% excess
  TP = TP1 / sum(TP1)
  TP = TP[, 1]
  tpreturn <<- t(TP) %*% (excess + riskfree)
  tpvola <<- sqrt(t(TP) %*% (Sigma %*% TP)) * sqrt(260)
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