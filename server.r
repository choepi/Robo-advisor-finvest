server <- function(input, output, session) {
  
  #data initialization
  assets_list <<- c("^SSMI","CSBGC0.SW","GC=F","BTC-USD","^GSPC","^TNX")
  asl <<- c("SMI","SWIBND","GOLD","BITCOIN","SNP500","USBND")
  fx <<-"CHF=X"
  fxn <<-"USDCHF"
  usdchf()
  dat_asset <<- readRDS("database_price.RDS")#database einlesen
  ren <<- readRDS("database_ren.RDS")#database einlesen
  riskfree <<- readRDS("riskfree.RDS")#riskfree einlesen
  time_now <- Sys.Date() 
  portfolio_s <<- c(1, 0, 1, 0, 0, 0)
  portfolio_s2 <<- c(1, 0, 1, 0, 0, 0)
  portfolio_w_F()
  dat_mvp_F()
  dat_tp_F()
  dat_mvp_rec_F()
  dat_tp_rec_F()
  
  #database updaten falls älter als 1,
  if ((time_now - as.Date(last(index(dat_asset[[4]])))) >= 1) {
    cache <- get_data()
    dat_asset <<- cache[[1]]
    ren <<- cache[[2]]
    riskfree<<- get_rf()
    saveRDS(riskfree, file = "riskfree.RDS")
    saveRDS(dat_asset, file = "database_price.RDS")
    saveRDS(ren, file = "database_ren.RDS")
  }
  
  
  #info serverfunktion
  hintjs(
    session,
    options = list("hintButtonLabel" = "Hope this hint was helpful"),
    events = list("onhintclose" = I('alert("Wasn\'t that hint helpful")'))
  )
  
  observeEvent(c(input$help1,input$help2),
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back"),
                       events = list())
  )
  
  
  
  output$portfolio1 <- renderPlot({
    for (i in 1:length(asl)) {
      portfolio_s[i] <<- input[[paste0("num", as.character(i))]]
    }
    
    portfolio_w_F()
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
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    w <- round(sum(portfolio_w), 1)
    paste("Portfolio Value:", w , "CHF")
  })
  
  
  output$mvp <- renderPlot({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    dat_mvp_F()
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
    
    dat_tp_F()
    
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
    if (input$slider2 == "1D") a <- 1 #2d da am sonntag 1tag == 0
    if (input$slider2 == "5D") a <- 5
    if (input$slider2 == "1M") a <- 30
    if (input$slider2 == "6M") a <- 180
    if (input$slider2 == "1Y") a <- 365
    if (input$slider2 == "5Y") a <- 5 * 365
    if (input$slider2 == "Max.") a <- 0
    
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
  
  
  output$mvprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    dat_mvp_rec_F()
    dat_mvp_rec
  })
  
  
  output$mvprec_inf <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    mvprec_inf <- data.frame("Volatilität"=round(mvpvola,2),
                                "Rendite"=round(mvpreturn,2))
    mvprec_inf
  })
  
  output$tprec_inf <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    tprec_inf <- data.frame("Volatilität"=round(tpvola,2),
                             "Rendite"=round(tpreturn,2))
    tprec_inf
  })
  
  
  output$tprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    dat_tp_rec_F()
    dat_tp_rec
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
    
    # normed.weights <- portfolio_s/sum(portfolio_s)
    # weighted.portfolio <<- normed.weights[1]*dat_asset[[1]][,4]+
    #   normed.weights[2]*dat_asset[[2]][,4]+
    #   normed.weights[3]*dat_asset[[3]][,4]+
    #   normed.weights[4]*dat_asset[[4]][,4]+
    #   normed.weights[5]*dat_asset[[5]][,4]+
    #   normed.weights[6]*dat_asset[[6]][,4]+
    #   normed.weights[7]*dat_asset[[7]][,4]
    # 
    #plot.xts(weighted.portfolio)
    dat_mvp_rec
    
    #weighted portfolio with just close for basic plot
    normed.weights <- portfolio_s
    weighted.portfolio <- dat_asset[[1]]
    for (i in 2:(length(asl)-1)){
      weighted.portfolio <- weighted.portfolio + normed.weights[i]*dat_asset[[i]]
    }
    #plot.xts(weighted.portfolio)
    #in column 4 is "close" of the chosen asset
    
    ##############################################
    names.ren <- c()
    for (i in ren) names.ren <- rbind(names.ren,colnames(i[,2]))
    
    weights_tp <- c(0,0,0,0,0,0,0)
    for (i in 1:length(names.ren)){
      q = names.ren[i]
      for(d in 1:length(dat_tp_rec[,1])){
        r <- dat_tp[,1][d]
        if (r==q) weights_tp[i] <- dat_tp_rec[d,4]
      }
    };print(dat_tp_rec);print(weights_tp)
    ###############################################
    
    
    start = as.Date(last(index(weighted.portfolio)))
    if (b == 0) dat <- window(weighted.portfolio, start = first(index(weighted.portfolio)), end=start)
    else if (b == 1 ) weighted.portfolio <- window(weighted.portfolio, start = start, end=start)
    else weighted.portfolio <- window(weighted.portfolio, start = start-b, end=start)
    
    
    if (input$radioHistorie == 1 & b == 1) {
      ggplot(data = weighted.portfolio[,4], aes(x = Index, y = Close))+
        geom_point(color = "green4")
    }
    else if (input$radioHistorie == 1 & b != 1){
      ggplot(data = weighted.portfolio[,4], aes(x = Index, y = Close)) +
        geom_line(color = "green4")
    }
    else if (input$radioHistorie == 2){
      chartSeries(weighted.portfolio ,name="Historie",theme = 'white')
    }
    
  })
  
  
  
  
  
  # output$vorschlag_diag({
  #   alpha = (-100:100) / 10 #alpha = 1 => tp, alpha get band von 0-1.5 durch
  #   walpha = (alpha %o% TP) + ((1 - alpha) %o% as.numeric(MVP))
  #   preturn = walpha %*% mittel
  #   pvola = c()
  #   for (i in 1:16) pvola[i] = sqrt(walpha[i,] %*% (Sigma_t %*% walpha[i,])) * sqrt(260);pvola
  #   portfolio_s2 <- portfolio_s2/sum(portfolio_s2)
  #   risk
  #   zu_invest_verm
  #   
  #   calculate_alpha(portfolio_s2,ren)
  #   
  #   
  # 
  # })
  
}

