

#Adjustedst index 
which.closest <- function(x,invect,index=T) {
  pick <- which.min(abs(invect-x))
  if (index) {
    return(pick)
  } else {
    return(invect[pick])
  }
} 
risk_F<-function(o){
  if (o == "Geringes Risiko")
    risk <<- 0
  else if (o == "Mittleres Risiko")
    risk <<- 1
  else if (o == "Hohes Risiko")
    risk <<- 2
}
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
  usd_chf <- suppressWarnings(getSymbols(fx, src = "yahoo", auto.assign = FALSE)[,6])
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
      chf_ts <- na.omit(merge(usd_ts,usd_chf))
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
    r <- na.omit(l[[i]][,6])
    r <- na.omit(dailyReturn(r,type = "arithmetic"))*260/100
    colnames(r) <-
      c(paste0(asl[i],"."))
    ren[[i]] <- r
    attributes(ren[[i]])$na.action <- NULL
  }
  data <- list(l,ren)
  return(data)
}


mvp <- function(y) {
  y <-window(y, start=Sys.Date()-age, end=Sys.Date())
  mvp <- minvariancePortfolio(as.timeSeries(y),spec =portfolioSpec() , constraints = "LongOnly")
  MVP <<- getWeights(mvp)
  N = dim(y)[1]
  mittel <<- t(y) %*% rep(1 / N, N) * 260
  Sigma = cov(y, y)
  
  mvpreturn <<- t(MVP) %*% mittel
  mvpvola <<- sqrt(t(MVP) %*% (Sigma %*% MVP)) * sqrt(260)
  return(as.array(MVP))
}


tp <- function(y) {
  y <-window(y, start=Sys.Date()-age, end=Sys.Date())
  N = dim(y)[1]
  excess = t(y) %*% rep(1 / N, N) * 260 - riskfree
  Sigma = cov(y, y)
  Portfolio1 <- as.timeSeries(y)
  
  
  cons <- "LongOnly"
  if (shortpara==T) {
    spec <- portfolioSpec()
    setSolver(spec) <- "solveRshortExact"
    setRiskFreeRate(spec) <- riskfree
    tanPort1 <- tangencyPortfolio(Portfolio1, spec=spec, constraints="Short")
  }else if (shortpara==F){
    spec <- portfolioSpec()
    setRiskFreeRate(spec) <- 0 #???
    tanPort1 <- tangencyPortfolio(Portfolio1, spec=spec, constraints="LongOnly")
  }
  
  #################################
  # library(fPortfolio)
  # y <- as.timeSeries(a)
  # Frontier<-portfolioFrontier(y)
  # plot(Frontier,1) # Plot EF
  # plot(Frontier,2) # Min. Risk Portfolio
  # plot(Frontier,3) # Tangency Portfolio
  # plot(Frontier,4) # Risk/Return of Single Assets
  # plot(Frontier,5) # Equal Weights Portfolio
  # plot(Frontier,6) # Two Asset Frontiers(Long Only)
  # plot(Frontier,7) # Monte Carlo Portfolios
  # plot(Frontier,8) # MArkowitz PF Only
  # riskReturnPoints<-frontierPoints(Frontier) #risk and return points on the EF
  # annualizedPoints<-data.frame(targetRisk=riskReturnPoints[,"targetRisk"]*sqrt(252),
  #                              targetReturn=riskReturnPoints[,"targetReturn"]*252)
  # plot(annualizedPoints)
  # 
  # riskFreeRate<-riskfree
  # plot((annualizedPoints[,"targetReturn"]-riskFreeRate)/annualizedPoints[,"targetRisk"], xlab="point on EF", ylab="Sharpe Ratio")
  # 
  # weightsPlot(Frontier)
  #################################
  
  TP<<-getWeights(tanPort1)
  tpreturn <<- t(TP) %*% (excess + riskfree)
  tpvola <<- sqrt(t(TP) %*% (Sigma %*% TP)) * sqrt(260)
  
  return(as.array(TP))
}


max <- function() {
  #risk = risik0, 5,9,14
  #worth sum == zu_invest_verm
  a <- xts()
  for (i in 1:length(portfolio_s2)) {
    if (portfolio_s2[i] > 0)
      a <- na.omit(merge(a, ren[[i]]))
  }
  y <-window(a, start=Sys.Date()-age, end=Sys.Date())
  N = dim(y)[1]
  mvp_m <- minvariancePortfolio(as.timeSeries(y),spec =portfolioSpec() , constraints = "LongOnly")
  MVP_m <<- getWeights(mvp_m)
  mittel_m <<- t(y) %*% rep(1 / N, N) * 260
  Sigma_m = cov(y, y)
  mvpreturn_m <<- t(MVP_m) %*% mittel_m
  mvpvola_m <<- sqrt(t(MVP_m) %*% (Sigma_m %*% MVP_m)) * sqrt(260)
  
  
  
  # alpha = seq(-1e4,1e3,1)/10 #alpha = 1 => tp, alpha get ban von 0-1.5 durch
  # walpha = (alpha %o% TP_m) + ((1 - alpha) %o% as.numeric(MVP_m))
  # preturn <- walpha%*%mittel_m
  # pvola <- c()
  # for (i in 1:length(alpha)){
  #   pvola[i] <- sqrt(walpha[i,] %*%(Sigma_m %*% walpha[i,]))*sqrt(260)
  # }
  # walpha <- as.data.frame(walpha)
  # walpha$alpha <- as.matrix(alpha)
  # walpha$preturn <- as.matrix(preturn)
  # walpha$pvola <- c(pvola)
  # walpha <- walpha/100
  # #plot(x = walpha$pvola,y <- walpha$preturn)
  # df <- walpha[which.min(abs((risk-walpha$preturn))),]
  # max_return <<- df$preturn
  # max_vola<<- df$pvola
  # m<-(df[ , -which(names(df) %in% c("alpha","pvola","preturn"))])
  # MAXX<<-as.matrix(m)
  # rownames(MAXX) <- NULL
  
  
  if (risk == 0) {
    max_return <<- mvpreturn_m
    max_vola <<- mvpvola_m
    MAXX <<- MVP_m
  } else if (risk == 1 | risk==2) {
    Portfolio2 <- as.timeSeries(y)
    excess_m = t(y) %*% rep(1 / N, N) * 260 - riskfree
    spec <- portfolioSpec()
    setRiskFreeRate(spec) <- 0 #???
    tanPort2 <- tangencyPortfolio(Portfolio2, spec=spec, constraints="LongOnly")
    TP_m<<-getWeights(tanPort2)
    tpreturn_m <<- t(TP_m) %*% (excess_m + riskfree)
    tpvola_m <<- sqrt(t(TP_m) %*% (Sigma_m %*% TP_m)) * sqrt(260)
    
    max_return <<- tpreturn_m
    max_vola <<- tpvola_m
    MAXX <<- TP_m
  }
  return((as.matrix(MAXX)))
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
    portfolio_w[i] <- portfolio_s[i] * last(dat_asset[[i]]$Adjusted)
  }
  portfolio_w <<- round((portfolio_w), 1)
}

portfolio_w_max_F <- function() {
  portfolio_w_max <- c()
  for (i in 1:length(portfolio_s2)) {
    portfolio_w_max[i] <- portfolio_s2[i] * last(dat_asset[[i]]$Adjusted)
  }
  portfolio_w_max <<- round((portfolio_w_max), 1)
  
  
}

dat_mvp_F <- function() {
  a <- xts()
  for (i in 1:length(portfolio_s)) {
    if (portfolio_s[i] > 0)
      a <- na.omit(merge(a, ren[[i]]))
  }
  dat_v <- mvp(a)
  
  dat_mvp <<- data.frame(Asset = rownames(dat_v),
                         Gewicht = c(dat_v))
  
}


dat_tp_F <- function(shortpara=F) {
  a <- xts()
  for (i in 1:length(portfolio_s)) {
    if (portfolio_s[i] > 0)
      a <- na.omit(merge(a, ren[[i]]))
  }
  shortpara <<- shortpara 
  
  dat_v <- tp(a)
  dat_tp <<- data.frame(Asset = rownames(dat_v),
                        Gewicht = c(dat_v))
  
}


dat_max_F <- function() {
  dat_v <- max()
  dat_max <<- data.frame(Asset = rownames(dat_v),
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
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Adjusted)
  }
  pa <- pa[pa != 0]
  dat_mvp_rec$Anzahl <- round(g / pa,1)
  n = length(dat_mvp_rec$Anzahl)
  h = c(rep(NA, n))
  for (i in 1:n) {
    if (round(portfolio_s[i] - dat_mvp_rec$Anzahl[i],2) < 0)
      h[i] <- "Kaufen"
    else if (round(portfolio_s[i] - dat_mvp_rec$Anzahl[i],2) > 0)
      h[i] <- "Verkaufen"
    else if (round(portfolio_s[i] - dat_mvp_rec$Anzahl[i],2) == 0)
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
    pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Adjusted)
  }
  pa <- pa[pa != 0]
  dat_tp_rec$Anzahl <- round(g / pa,1)
  n = length(dat_tp_rec$Anzahl)
  h = c(rep(NA, n))
  for (i in 1:n) {
    if (round(portfolio_s[i] - dat_tp_rec$Anzahl[i],2) < 0)
      h[i] <- "Kaufen"
    else if (round(portfolio_s[i] - dat_tp_rec$Anzahl[i],2) > 0)
      h[i] <- "Verkaufen"
    else if (round(portfolio_s[i] - dat_tp_rec$Anzahl[i],2) == 0)
      h[i] <- "Halten"
  }
  dat_tp_rec$Handlung <- h
  dat_tp_rec <-
    dat_tp_rec[order(abs(dat_tp_rec$Investiert), decreasing = T), ]
  dat_tp_rec <<- dat_tp_rec
}

dat_max_rec_F <- function() {
  dat_max_rec <- dat_max
  dat_max_rec$Gewicht <- round(dat_max$Gewicht, 2)
  as.data.frame(dat_max_rec)
  g <- c(portfolio_s2)
  g <- g[g != 0]
  lp <- c(1:length(g))
  sg <- zu_invest_verm
  abssum<- sum(abs(dat_max[lp, 2]))
  g <- sg/abssum*dat_max[lp, 2]
  
  dat_max_rec$Investiert <- (g)
  
  names.max <- c()
  for (i in ren) names.max <- rbind(names.max,colnames(i))
  weights_max <- c(0,0,0,0,0,0)
  for (i in 1:length(names.max)){
    q = names.max[i]
    for(d in 1:length(dat_max[,1])){
      r <- dat_max[,1][d]
      if (r==q) weights_max[i] <- dat_max[d,2]
    }
  }#;print(g);print(weights_max)
  
  pa <- weights_max
  for (i in 1:length(asl)) {
    pa[i] <- portfolio_s2[i]*last(dat_asset[[i]]$Adjusted)
  }
  pa <- pa[pa != 0]
  dat_max_rec$Anzahl <- round(g / pa,3)
  n = length(dat_max_rec$Anzahl)
  h = c(rep(NA, n))
  for (i in 1:n) {
    if (dat_max_rec$Anzahl[i] > 0)
      h[i] <- "Kaufen"
    else if (dat_max_rec$Anzahl[i] < 0)
      h[i] <- "Verkaufen"
    else if (dat_max_rec$Anzahl[i] == 0)
      h[i] <- "Nicht Kaufen"
  }
  dat_max_rec$Handlung <- h
  dat_max_rec <-
    dat_max_rec[order(abs(dat_max_rec$Investiert), decreasing = T), ]
  dat_max_rec <<- dat_max_rec
}


weightened.portfolio_F <- function(b){
  start = Sys.Date()
  end = start-b
  if (wday(start, label = T) == "Sa")start <- start-1
  if (wday(start, label = T) == "So")start <- start-2
  
  #gewünschter zeit horizont
  if (b == 1 ){
    w = start
  }else{
    w = end
  }
  
  dat <- list()
  for (i in 1:length(asl)){
    dat[[i]] <- window(dat_asset[[i]],start=end,end=start)
  }
  #weightened portfolio basic
  ######################
  
  c.old <- rep(0,length(b))
  for(s in 1:length(portfolio_w)){
    n <- which.closest(w,index(dat[[s]]),index = F)
    if (portfolio_w[s]!=0) c.old[s] <- coredata(dat[[s]][n,4]) 
  };c.old
  weight <- portfolio_w
  for (i in 1:length(portfolio_w)){
    if (portfolio_w[i]!=0){
      weight[i] <- weight[i]/c.old[i]
    }
  }
  ######################
  weightened.portfolio <- 0 #dat[[1]]
  for (i in 1:(length(asl))){
    weightened.portfolio <- weightened.portfolio + weight[i]*dat[[i]]
  }
  weightened.portfolio <- na.omit(weightened.portfolio)
  
  
  weightened.portfolio <<- window(weightened.portfolio, start = end, end=start)
  
  
  #weightened portfolio TP
  names.ren.tp <- c()
  for (i in ren) names.ren.tp <- rbind(names.ren.tp,colnames(i[,1]))
  weights_tp <- c(0,0,0,0,0,0)
  
  for (i in 1:length(names.ren.tp)){
    q = names.ren.tp[i]
    for(d in 1:length(dat_tp_rec[,1])){
      r <- dat_tp[,1][d]
      if (r==q) weights_tp[i] <- dat_tp_rec[d,3]
    }
  }#;print(dat_tp_rec);print(weights_tp)
  
  ######################
  c.old <- rep(0,length(weights_tp))
  for(s in 1:length(names.ren.tp)){
    n <- which.closest(w,index(dat[[s]]),index = F)
    if (weights_tp[s]!=0) c.old[s] <- coredata(dat[[s]][n,4]) 
  };c.old
  for (i in 1:length(weights_tp)){
    if (weights_tp[i]!=0){
      weights_tp[i] <- weights_tp[i]/c.old[i]
    }
  }
  ######################
  
  weightened.portfolio.tp <- 0 #dat[[1]]
  for (i in 1:(length(asl))){
    weightened.portfolio.tp <- weightened.portfolio.tp + weights_tp[i]*dat[[i]]
  }
  weightened.portfolio.tp <- na.omit(weightened.portfolio.tp)
  weightened.portfolio.tp <<- window(weightened.portfolio.tp, start = end, end=start)
  
  #weightened portfolio MVP
  names.ren.mvp <- c()
  for (i in ren) names.ren.mvp <- rbind(names.ren.mvp,colnames(i[,1]))
  weights_mvp <- c(0,0,0,0,0,0)
  for (i in 1:length(names.ren.mvp)){
    q = names.ren.mvp[i]
    for(d in 1:length(dat_mvp_rec[,1])){
      r <- dat_mvp[,1][d]
      if (r==q) weights_mvp[i] <- dat_mvp_rec[d,3]
    }
  }#;print(dat_mvp_rec);print(weights_mvp)
  
  
  ######################
  c.old <- rep(0,length(weights_mvp))
  for(s in 1:length(names.ren.mvp)){
    n <- which.closest(w,index(dat[[s]]),index = F)
    if (weights_mvp[s]!=0) c.old[s] <- coredata(dat[[s]][n,4]) 
  };c.old
  
  for (i in 1:length(weights_mvp)){
    if (weights_mvp[i]!=0){
      weights_mvp[i] <- weights_mvp[i]/c.old[i]
    }
  }
  ######################
  
  
  weightened.portfolio.mvp <- 0 #dat[[1]]
  for (i in 1:(length(asl))){
    weightened.portfolio.mvp  <- weightened.portfolio.mvp  + weights_mvp[i]*dat[[i]]
  }
  weightened.portfolio.mvp <- na.omit(weightened.portfolio.mvp)
  
  weightened.portfolio.mvp <<- window(weightened.portfolio.mvp, start = end, end=start)
}


weightened.portfolio2_F <- function(b){
  
  start = Sys.Date()
  end = start-b
  if (wday(start, label = T) == "Sa")start <- start-1
  if (wday(start, label = T) == "So")start <- start-2
  
  #gewünschter zeit horizont
  if (b == 1 ){
    w = start
  }else{
    w = end
  }
  
  dat <- list()
  for (i in 1:length(asl)){
    dat[[i]] <- window(dat_asset[[i]],start=end,end=start)
  }
  # #weightened portfolio MAXX
  
  names.ren.max <- c()
  v <- dat_max_rec[order(as.numeric(rownames(dat_max_rec))),,drop=FALSE]
  
  for (i in ren) names.ren.max <- rbind(names.ren.max,colnames(i[,1]))
  weights_max <- c(0,0,0,0,0,0)
  for (i in 1:length(names.ren.max)){
    q = names.ren.max[i]
    for(d in 1:length(v[,1])){
      r <- dat_max[,1][d]
      if (r==q) weights_max[i] <- v[d,3]
    }
  }#;print(v);print(weights_max)
  
  
  ######################
  c.old <- rep(0,length(weights_max))
  for(s in 1:length(names.ren.max)){
    n <- which.closest(w,index(dat[[s]]),index = F)
    if (weights_max[s]!=0) c.old[s] <- coredata(dat[[s]][n,4]) 
  };c.old
  
  for (i in 1:length(weights_max)){
    if (weights_max[i]!=0){
      weights_max[i] <- weights_max[i]/c.old[i]
    }
  }
  ######################
  
  
  weightened.portfolio.max <- 0 #dat[[1]]
  for (i in 1:(length(asl))){
    weightened.portfolio.max  <- weightened.portfolio.max  + weights_max[i]*dat[[i]]
  }
  weightened.portfolio.max <- na.omit(weightened.portfolio.max)
  
  weightened.portfolio.max <<- window(weightened.portfolio.max, start = end, end=start)
}


plot_forecast<-function(xs,q,name_a){
  auscafe <-as.ts(xs)
  a <- coredata(xs)
  train <- auscafe
  h <- q
  
  ETS <- forecast(ets(train), h=h)
  ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),h=h)
  NNAR <- forecast(nnetar(train), h=h)
  TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
  Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                    NNAR[["mean"]] + TBATS[["mean"]])/4
  
  st_d <- index(xs[(length(xs)-10):(length(xs)-1)])
  start_date <- index(xs[length(xs)])
  end_date <- start_date+h
  date_vector <- seq(start_date, end_date, by = "day")
  date_vector_no_weekend <- date_vector[!(weekdays(date_vector) %in% c("Saturday", "Sunday"))]
  d_ind <- append(st_d,date_vector_no_weekend)
  yl <- seq(length(xs)-10,(length(xs)+h),1)
  
  d_max <-coredata(c(ETS$mean,ARIMA$mean,NNAR$mean,TBATS$mean,Combination))
  
  max_1 <- last(d_max[order(d_max,decreasing = F)])+100
  min_1 <- last(d_max[order(d_max,decreasing = T)])-100
  max_2 <- first(coredata(xs[(length(xs)-10):length(xs)])[order(xs[(length(xs)-10):length(xs)],decreasing = T)]) + 100
  min_2<- first(coredata(xs[(length(xs)-10):length(xs)])[order(xs[(length(xs)-10):length(xs)],decreasing = F)]) - 100
  ymin <- 0
  ymax <- 0
  if (min_1>min_2) ymin <- min_2 else ymin <- min_1
  if (max_2>max_1) ymax <- max_2 else ymax <- max_2
  
  p <- autoplot(auscafe) +
    autolayer(ETS, series = "ETS", PI = FALSE) +
    autolayer(ARIMA, series = "ARIMA", PI = FALSE) +
    autolayer(NNAR, series = "NNAR", PI = FALSE) +
    autolayer(TBATS, series = "TBATS", PI = FALSE) +
    autolayer(Combination, series = "Combination", linewidth = 3) +
    ylab("CHF") +
    xlab(d_ind)+
    xlim(c(length(a) - 10, length(a) + 40))+
    scale_y_continuous( limits = c(ymin,ymax))+
    ggtitle(name_a)

  return(p)
}





