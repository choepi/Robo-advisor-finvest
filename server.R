server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$select2)
  })
  
  


  output$historical_data <- renderPlot({
    assetlist <- list("SMI" =1,"SWIBND" = 2,
                      "GOLD"=3,"BITCOIN"=4,
                      "SNP500"=5,"USBND"=6,
                      "USDCHF"=6)
    a <- (input$dateinput1)-Sys.Date()
    l <- get_data(a)
    chose <- as.numeric(assetlist[input$select2])
    dat <- l[[chose]]
    autoplot(dat, geom = "line", xlim = c(input$dateinput1, Sys.Date()))
    #ggplot(data = dat, mapping = aes(x = Index, y = Price))+
    #  geom_line()
  })
  
}