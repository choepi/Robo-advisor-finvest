server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  dat_asset <- get_data(1000) #data initialization
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$select2)
  })
  
  
  
  
  output$historical_data <- renderPlot({
    a <- (input$dateinput1)-Sys.Date()
    if (a>1000) dat_asset <- get_data(a)
    
    assetlist <- list("SMI" =1,"SWIBND" = 2,
                      "GOLD"=3,"BITCOIN"=4,
                      "SNP500"=5,"USBND"=6,
                      "USDCHF"=7)
    cnames <- names(assetlist)
    chose <- as.numeric(assetlist[input$select2])
    dat <- as.xts(dat_asset[[chose]])
    dat <- window(dat, start = input$dateinput1, end=Sys.Date())
    #ggplot(data = dat$Close, aes(x = Index, y = Close))+
      #geom_line()
    chartSeries(dat,name=cnames[chose],theme = 'white')
  })
  
}




      
