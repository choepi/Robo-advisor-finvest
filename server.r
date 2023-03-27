
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  #data initialization
  dat_asset <<- get_data() 
  time_now <- Sys.time()
  whichasset <- c(1,1,1,1,1,0,0)
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$select2)
  })
  
  
  output$historical_data <- renderPlot({
    if (input$slider2=="1D") a <- 1 #2d da am sonntag 1tag == 0
    if (input$slider2=="5D") a <- 5
    if (input$slider2=="1M") a <- 30
    if (input$slider2=="6M") a <- 180
    if (input$slider2=="1Y") a <- 365
    if (input$slider2=="5Y") a <- 5*365
    if (input$slider2=="Max.") a <- 0
    if (abs(time_now-Sys.time())>300) dat_asset <- get_data() #refresh nach 300s
    
    assetlist <- list("SMI" =1,"SWIBND" = 2,
                      "GOLD"=3,"BITCOIN"=4,
                      "SNP500"=5,"USBND"=6,
                      "USDCHF"=7)
    cnames <- names(assetlist)
    chose <- as.numeric(assetlist[input$select2])
    dat <- as.xts(dat_asset[[chose]])
    if (a ==0) dat <- window(dat, start = Sys.Date()-nrow(dat), end=Sys.Date())
    if (a ==1 ) dat <- window(dat, start = last(index(dat)), end=Sys.Date())
    else dat <- window(dat, start = Sys.Date()-a, end=Sys.Date())
    
    
    if (input$radio1 == 1 & a == 1) {
      ggplot(data = dat$Close, aes(x = Index, y = Close))+
        geom_point()
    }
    else if (input$radio1 == 1 & a != 1){
      ggplot(data = dat$Close, aes(x = Index, y = Close))+
        geom_line()
    }
    else if (input$radio1 == 2){
      chartSeries(dat,name=cnames[chose],theme = 'white')
    }
  })
  
  
  output$mvp <- renderPlot({
    dat_v <- mvp(whichasset)
    dat_mvp <- data.frame(
      group=colnames(dat_v),
      value=c(dat_v)
    )
    ggplot(dat_mvp, aes(x="", y=value, fill=group)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() # remove background, grid, numeric labels
    
    
    
  })
}



