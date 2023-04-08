server <- function(input, output, session) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  #data initialization
  assets_list <<-
    c("^SSMI",
      "CSBGC0.SW",
      "GC=F",
      "BTC-USD",
      "^GSPC",
      "^TNX",
      "CHF=X")
  asl <<- c("SMI", "SWIBND", "GOLD", "BITCOIN", "SNP500", "USBND", "USD")
  dat_asset <<- readRDS("database.RDS") #database einlesen
  time_now <- Sys.Date()
  #database updaten falls älter als 1,
  if ((time_now - last(index(dat_asset[[4]]))) >= 1) {
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
  
  observeEvent(input$help,
               introjs(
                 session,
                 options = list(
                   "showBullets" = "false",
                   "showProgress" = "true",
                   "showStepNumbers" = "false",
                   "nextLabel" = "Next",
                   "prevLabel" = "Prev",
                   "skipLabel" = "Skip"
                 )
               ))
  
  
  
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
  }, bg="transparent")
  
  
  
  output$portfolio_worth1 <- renderText({
    #reactive auf inputs
    portfolio_w <<- portfolio_s
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
    
    for (i in 1:length(asl)) {
      portfolio_w[i] <- portfolio_s[i] * last(dat_asset[[i]]$Close)
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
    
    dat_mvp <- data.frame(group = rownames(dat_v),
                          value = c(dat_v))
    ggplot(dat_mvp, aes(x = "", y = value, fill = group)) +
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
    
    dat_tp <- data.frame(group = rownames(dat_v),
                         value = c(dat_v))
    ggplot(dat_tp, aes(x = "", y = value, fill = group)) +
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
    start = last(index(dat))
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
  
}