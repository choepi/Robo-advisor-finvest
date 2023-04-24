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
    input$shortpara
    dat_tp_F(input$shortpara)
    
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
  
  
  #Plot Protfolio time series
  output$weighted.portfolio <- renderPlot({
    if (input$sliderHistorie=="1D") b <- 1 #2d da am sonntag 1tag == 0
    if (input$sliderHistorie=="5D") b <- 5
    if (input$sliderHistorie=="1M") b <- 30
    if (input$sliderHistorie=="6M") b <- 180
    if (input$sliderHistorie=="1Y") b <- 365
    if (input$sliderHistorie=="5Y") b <- 5*365
    if (input$sliderHistorie=="10Y") b <- 10*365
    dat_mvp_F()
    dat_tp_F()
    dat_mvp_rec_F()
    dat_tp_rec_F()
    
    for (i in 1:length(portfolio_s)){
      input[[paste0("num", as.character(i))]]
    }
    
    start = Sys.Date()  #gewünschter zeit horizont
    if (b == 1 ){
      w = start
    }else{
      w = start-b
    }
    
    #weighted portfolio basic
    ######################

    c.old <- rep(0,length(portfolio_w))
    for(s in 1:length(portfolio_w)){
      if (portfolio_w[s]>0) c.old[s] <- coredata(dat_asset[[s]][w,4]) 
    };c.old
    weight <- portfolio_w
    for (i in 1:length(portfolio_w)){
      if (portfolio_w[i]>0){
        weight[i] <- weight[i]/c.old[i]
      }
    }
    ######################
    weighted.portfolio <- 0 #dat_asset[[1]]
    for (i in 1:(length(asl))){
      weighted.portfolio <- weighted.portfolio + weight[i]*dat_asset[[i]]
    }
    weighted.portfolio <- na.omit(weighted.portfolio)
    
    
    if (b == 1 ){
      weighted.portfolio <- window(weighted.portfolio, start = start, end=start)
    }else{
      weighted.portfolio <- window(weighted.portfolio, start = start-b, end=start)
    }
    
    #weighted portfolio TP
    names.ren.tp <- c()
    for (i in ren) names.ren.tp <- rbind(names.ren.tp,colnames(i[,2]))
    weights_tp <- c(0,0,0,0,0,0,0)
    
    for (i in 1:length(names.ren.tp)){
      q = names.ren.tp[i]
      for(d in 1:length(dat_tp_rec[,1])){
        r <- dat_tp[,1][d]
        if (r==q) weights_tp[i] <- dat_tp_rec[d,3]
      }
    };print(dat_tp_rec);print(weights_tp)
    
    ######################
    c.old <- rep(0,length(weights_tp))
    for(s in 1:length(names.ren.tp)){
      if (weights_tp[s]>0) c.old[s] <- coredata(dat_asset[[s]][w,4]) 
    };c.old
    
    for (i in 1:length(weights_tp)){
        if (weights_tp[i]>0){
          weights_tp[i] <- weights_tp[i]/c.old[i]
        }
    }
    ######################
    
    weighted.portfolio.tp <- 0 #dat_asset[[1]]
    for (i in 1:(length(asl))){
      weighted.portfolio.tp <- weighted.portfolio.tp + weights_tp[i]*dat_asset[[i]]
    }
    weighted.portfolio.tp <- na.omit(weighted.portfolio.tp)

    if (b == 1 ){
      weighted.portfolio.tp <- window(weighted.portfolio.tp, start = start, end=start)
    }else{
      weighted.portfolio.tp <- window(weighted.portfolio.tp, start = start-b, end=start)
    }
    #weighted portfolio MVP
    names.ren.mvp <- c()
    for (i in ren) names.ren.mvp <- rbind(names.ren.mvp,colnames(i[,2]))
    weights_mvp <- c(0,0,0,0,0,0,0)
    for (i in 1:length(names.ren.mvp)){
      q = names.ren.mvp[i]
      for(d in 1:length(dat_mvp_rec[,1])){
        r <- dat_mvp[,1][d]
        if (r==q) weights_mvp[i] <- dat_mvp_rec[d,3]
      }
    };print(dat_mvp_rec);print(weights_mvp)
    
    
    ######################
    c.old <- rep(0,length(weights_mvp))
    for(s in 1:length(names.ren.mvp)){
      if (weights_mvp[s]>0) c.old[s] <- coredata(dat_asset[[s]][w,4]) 
    };c.old
    
    for (i in 1:length(weights_mvp)){
      if (weights_mvp[i]>0){
        weights_mvp[i] <- weights_mvp[i]/c.old[i]
      }
    }
    ######################
    
    
    weighted.portfolio.mvp <- 0 #dat_asset[[1]]
    for (i in 1:(length(asl))){
      weighted.portfolio.mvp  <- weighted.portfolio.mvp  + weights_mvp[i]*dat_asset[[i]]
    }
    weighted.portfolio.mvp <- na.omit(weighted.portfolio.mvp)
    
    if (b == 1 ){
      weighted.portfolio.mvp <- window(weighted.portfolio.mvp, start = start, end=start)
    }else {
      weighted.portfolio.mvp <- window(weighted.portfolio.mvp, start = start-b, end=start)
    }
    
    if (input$radioHistorie == 1 & b == 1) {
      ggplot(data = weighted.portfolio[,4], aes(x = Index, y = Close))+
        geom_point(color = "green4")+
        geom_point(data = weighted.portfolio.mvp[,4], color = "blue")+
        geom_point(data = weighted.portfolio.tp[,4], color = "red")
    }
    else if (input$radioHistorie == 1 & b != 1){
      ggplot(data = weighted.portfolio[,4], aes(x = Index, y = Close)) +
        geom_line(color = "green4")+
        geom_line(data = weighted.portfolio.mvp[,4], aes(x = Index, y = Close), color = "blue")+
        geom_line(data = weighted.portfolio.tp[,4], aes(x = Index, y = Close), color = "red")
    }
    else if (input$radioHistorie == 2){
      chartSeries(weighted.portfolio ,name="Historie",theme = 'white')
    }
    
  })
  
  
  output$mvprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    dat_mvp_rec_F()
    dat_mvp_rec
  })
  
  
  output$tprec <- renderTable({
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    dat_tp_rec_F()
    dat_tp_rec
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
  
  

  
}
