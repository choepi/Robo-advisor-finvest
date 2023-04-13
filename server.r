erver <- function(input, output, session) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  #data initialization
  assets_list <<- c("^SSMI","CSBGC0.SW","GC=F","BTC-USD","^GSPC","^TNX","CHF=X")
  asl <<- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND","USDCHF")
  dat_asset <<- readRDS("database.RDS") #database einlesen
  time_now <- Sys.Date()
  #database updaten falls älter als 1,
  if ((time_now - as.Date(last(index(dat_asset[[4]])))) >= 1) {
    dat_asset <<- get_data()
    saveRDS(dat_asset, file = "database.RDS")
  }
  portfolio_s <<- c(1, 0, 1, 0, 0, 0, 0)
  portfolio_s2 <<- c(1, 0, 0, 0, 0, 0, 0)
  portfolio_w <<- 0
  
  #info serverfunktion
  hintjs(
    session,
    options = list("hintButtonLabel" = "Hope this hint was helpful"),
    events = list("onhintclose" = I('alert("Wasn\'t that hint helpful")'))
  )
  
  
  output$portfolio1 <- renderPlot({
    for (i in 1:length(portfolio_s)) {
      portfolio_s[i] <<- input[[paste0("num", as.character(i))]]
    }
    for (i in 1:length(asl)) {
      portfolio_w[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
    }
    portfolio_w <- round((portfolio_w), 0)
    
    dat_v <- as.matrix(t(portfolio_w))
    colnames(dat_v) <- asl
    dat_port <- data.frame(group = colnames(dat_v),
                           value = c(dat_v))
    ggplot(dat_port, aes(x = "", y = value, fill = group)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void() # remove background, grid, numeric labels
  })
  
  
  output$portfolio2 <-  renderPlot({
    for (i in 1:(length(portfolio_s2))) {
      portfolio_s2[i] <<- if (input[[paste0("checkbox", as.character(i))]]) 1
    }
    
    if (input$slider3 == "Geringes Risiko")
      risk <<- 1
    else if (input$slider3 == "Mittleres Risiko")
      risk <<- 2
    else if (input$slider3 == "Hohes Risiko")
      risk <<- 3
    
    zu_invest_verm <<- input$num15
    
    dat_v <- as.matrix(t(portfolio_s2))
    colnames(dat_v) <- asl
    dat_port <- data.frame(group = colnames(dat_v),
                           value = c(dat_v))
    ggplot(dat_port, aes(x = "", y = value, fill = group)) +
      geom_bar(stat = "identity",
               width = 1) +
      coord_polar("y", start = 0) +
      theme_void() # remove background, grid, numeric labels
  })
  
  
  output$portfolio_worth1 <- renderText({
    #reactive auf inputs
    portfolio_w <<- portfolio_s
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    for (i in 1:length(asl)) {
      portfolio_w[i] <<- portfolio_s[i] * last(dat_asset[[i]]$Close)
    }
    w <- round(sum(portfolio_w), 0)
    paste("Portfolio Value:", w , "$")
  })
  
  
  output$mvp <- renderPlot({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    a <- data.frame()
    for (i in 1:length(portfolio_s)) {
      if (portfolio_s[i] > 0)
        a <- cbind.fill(a, dat_asset[[i]][,7])
    }
    dat_v <- mvp(a)
    
    dat_mvp <<- data.frame(Asset = rownames(dat_v),
                           Gewicht = c(dat_v))
    ggplot(dat_mvp, aes(x = "", y = Gewicht, fill = Asset)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  
  output$tp <- renderPlot({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    a <- data.frame()
    for (i in 1:length(portfolio_s)) {
      if (portfolio_s[i] > 0)
        a <- cbind.fill(a, dat_asset[[i]][,7])
    }
    
    riskfree <<- 0.01
    dat_v <- tp(a)
    
    dat_tp <<- data.frame(Asset = rownames(dat_v),
                          Gewicht = c(dat_v))
    ggplot(dat_tp, aes(x = "", y = Gewicht, fill = Asset)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  
  output$selected_var <- renderText({ 
    paste("You have selected", asl[as.numeric(input$select2)])
  })
  
  
  output$historical_data <- renderPlot({
    if (input$slider2 == "1D")
      a <- 1 #2d da am sonntag 1tag == 0
    if (input$slider2 == "5D")
      a <- 5
    if (input$slider2 == "1M")
      a <- 30
    if (input$slider2 == "6M")
      a <- 180
    if (input$slider2 == "1Y")
      a <- 365
    if (input$slider2 == "5Y")
      a <- 5 * 365
    if (input$slider2 == "Max.")
      a <- 0
    
    
    
    chose <<- as.numeric(input$select2)
    dat <- as.xts(dat_asset[[chose]])
    start = as.Date(last(index(dat)))
    if (a == 0)
      dat <- window(dat, start = first(index(dat)), end = start)
    else if (a == 1)
      dat <- window(dat, start = start, end = start)
    
    else
      dat <- window(dat, start = start - a, end = start)
    
    if (input$radio1 == 1 & a == 1) {
      ggplot(data = dat$Close, aes(x = Index, y = Close)) +
        geom_point()
    }
    else if (input$radio1 == 1 & a != 1) {
      ggplot(data = dat$Close, aes(x = Index, y = Close)) +
        geom_line()
    }
    else if (input$radio1 == 2) {
      chartSeries(dat, name = asl[chose], theme = 'white')
    }
  })
  
  
  #Plot Protfolio time series
  output$weighted.portfolio <- renderPlot({
    if (input$sliderHistorie=="1D") b <- 1 #2d da am sonntag 1tag == 0
    if (input$sliderHistorie=="5D") b <- 5
    if (input$sliderHistorie=="1M") b <- 30
    if (input$sliderHistorie=="6M") b <- 180
    if (input$sliderHistorie=="1Y") b <- 365
    if (input$sliderHistorie=="5Y") b <- 5*365
    if (input$sliderHistorie=="Max.") b <- 0
    
    
    for (i in 1:length(portfolio_s)){
      input[[paste0("num", as.character(i))]]
    }
    normed.weights <- portfolio_s/sum(portfolio_s)
    weighted.portfolio <<- normed.weights[1]*dat_asset[[1]][,4]+
      normed.weights[2]*dat_asset[[2]][,4]+
      normed.weights[3]*dat_asset[[3]][,4]+
      normed.weights[4]*dat_asset[[4]][,4]+
      normed.weights[5]*dat_asset[[5]][,4]+
      normed.weights[6]*dat_asset[[6]][,4]+
      normed.weights[7]*dat_asset[[7]][,4]
    
    #plot.xts(weighted.portfolio)
    start = as.Date(last(index(weighted.portfolio)))
    if (b == 0) dat <- window(weighted.portfolio, start = first(index(weighted.portfolio)), end=start)
    else if (b == 1 ) weighted.portfolio <- window(weighted.portfolio, start = start, end=start)
    else weighted.portfolio <- window(weighted.portfolio, start = start-b, end=start)
    
    if (b == 1) {
      ggplot(data = weighted.portfolio, aes(x = Index, y = Close))+
        geom_point(color = "green4")
    }
    else if (b != 1){
      ggplot(data = weighted.portfolio, aes(x = Index, y = Close)) +
        geom_line(color = "green4")
    }
    
  })
  
  output$mvprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    dat_mvp_rec <- dat_mvp
    dat_mvp_rec$Gewicht <- dat_mvp$Gewicht * 100
    as.data.frame(dat_mvp_rec)
    g <- c(portfolio_w)
    g <- g[g != 0]
    lp <- c(1:length(g))
    g <- sum(g)
    g <- g * dat_mvp[lp, 2]
    dat_mvp_rec$Investiert <- g
    pa <- portfolio_w
    for (i in 1:length(asl)) {
      pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
    }
    pa <- pa[pa != 0]
    dat_mvp_rec$Anzahl <- round(dat_mvp_rec$Investiert/pa,0)
    dat_mvp_rec
  })
  
  
  output$tprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    dat_tp_rec <- dat_tp
    dat_tp_rec$Gewicht <- dat_tp$Gewicht * 100
    as.data.frame(dat_tp_rec)
    g <- c(portfolio_w)
    g <- g[g != 0]
    lp <- c(1:length(g))
    g <- sum(g)
    g <- g * dat_tp[lp, 2]
    dat_tp_rec$Investiert <- g
    pa <- portfolio_w
    for (i in 1:length(asl)) {
      pa[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
    }
    pa <- pa[pa != 0]
    dat_tp_rec$Anzahl <- round(dat_tp_rec$Investiert/pa,0)
    dat_tp_rec
  })
  
}
