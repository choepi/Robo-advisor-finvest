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
  age <<- 5*365 #gedächtnis mvp/tp
  portfolio_s <<- c(1, 0, 1, 0, 1, 0)
  portfolio_s2 <<- c(1, 0, 1, 0, 1, 0)
  zu_invest_verm <<- 1000
  risk_F("Geringes Risiko")
  portfolio_w_F()
  dat_mvp_F()
  dat_mvp_rec_F()
  dat_tp_F()
  dat_tp_rec_F()
  dat_max_F()
  dat_max_rec_F()
  
  
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
  
  #reactiv function for num inputs
  inputs_num<- function(){
    for (i in 1:length(portfolio_s)) {
      input[[paste0("num", as.character(i))]]
    }
  }
  #reactive function for checkbox
  input_ckbx <- function(){
    for (i in 1:length(portfolio_s2)) {
      input[[paste0("checkbox", as.character(i))]]
    }
    for (i in 1:(length(portfolio_s2))) {
      if (input[[paste0("checkbox", as.character(i))]]) portfolio_s2[i] <<- 1
      else portfolio_s2[i] <<- 0
    }
  }
  input_slid3 <- function(){
    a <- input$slider3
    input$num15
    return(a)
  }
  #info serverfunktion
  output$portfolio1 <- renderPlot({
    inputs_num()
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
      theme_void() + # remove background, grid, numeric labels
      scale_fill_manual(values = c("#1a3a37","#038838", "#53c1b4", "#67d555", "#B2D800", "#027b76"))
  })
  
  
  output$portfolio_worth1 <- renderText({
    inputs_num()
    w <- round(sum(portfolio_w), 1)
    paste("Portfolio Value:", "CHF", format(w, big.mark = "\'"))
  })
  
  
  output$Asset1 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[1], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  output$Asset2 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[2], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  output$Asset3 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[3], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  output$Asset4 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[4], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  output$Asset5 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[5], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  output$Asset6 <- renderText({
    inputs_num()
    paste("CHF", format(portfolio_w[6], big.mark = "'", decimal.mark = ".", nsmall = 2))
  })
  
  
  
  output$mvp <- renderPlot({
    inputs_num()
    dat_mvp_F()
    dat_mvp_rec_F()
    ggplot(dat_mvp, aes(x = "", y = Gewicht, fill = Asset)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("#1a3a37","#038838", "#53c1b4", "#67d555", "#B2D800", "#027b76"))
  })
  
  
  output$tp <- renderPlot({
    inputs_num()
    dat_tp_F(input$shortpara)
    dat_mvp_F()
    dat_mvp_rec_F()
    dat_tp_rec_F()
    ggplot(dat_tp, aes(x = "", y = Gewicht, fill = Asset)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("#1a3a37","#038838", "#53c1b4", "#67d555", "#B2D800", "#027b76"))
  })
  
  output$max <-  renderPlot({
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    dat_max_F()
    dat_max_rec_F()
    
    
    
    
    ggplot(dat_max, aes(x = "", y = Gewicht, fill = Asset)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("#1a3a37","#038838", "#53c1b4", "#67d555", "#B2D800", "#027b76"))
  })
  
  output$mvp2 <- renderPlot({
    inputs_num()
    dat_mvp_F()
    dat_mvp_rec_F()
    
    ggplot(dat_mvp, 
           aes(x = Asset, 
               y = Gewicht, 
               fill = Gewicht < 0)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(guide = FALSE,
                        name = 'Gewicht < 0', 
                        values = setNames(c('limegreen', "red"), c(F, T)))
    
    
    
    # ggplot(dat_mvp, aes(x = Gewicht, y = Asset)) +
    #   geom_col(fill = "#0099f9") +
    #   coord_flip()
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", asl[as.numeric(input$select2)])
  })
  
  output$max2 <-  renderPlot({
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    dat_max_F()
    dat_max_rec_F()
    ggplot(dat_max, 
           aes(x = Asset, 
               y = Gewicht, 
               fill = Gewicht < 0)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(guide = FALSE,
                        name = 'Gewicht < 0', 
                        values = setNames(c('limegreen', 'red'), c(F, T)))
    # ggplot(dat_max, aes(x = Gewicht, y = Asset)) +
    #   geom_col(fill = "#0099f9") +
    #   coord_flip()
  })
  
  output$tp2 <- renderPlot({
    inputs_num()
    dat_tp_F(input$shortpara)
    dat_mvp_F()
    dat_mvp_rec_F()
    dat_tp_rec_F()
    ggplot(dat_tp, 
           aes(x = Asset, 
               y = Gewicht, 
               fill = Gewicht < 0)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(guide = FALSE,
                        name = 'Gewicht < 0', 
                        values = setNames(c('limegreen', 'red'), c(F, T)))
    # ggplot(dat_tp, aes(x = Gewicht, y = Asset)) +
    #   geom_col(fill = "#0099f9") +
    #   coord_flip()
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
      ggplot(data = dat$Adjusted, aes(x = Index, y = Adjusted)) +
        geom_point()
    }
    else if (input$radio1 == 1 & a != 1) {
      ggplot(data = dat$Adjusted, aes(x = Index, y = Adjusted)) +
        geom_line()
    }
    else if (input$radio1 == 2) {
      chartSeries(dat, name = asl[chose], theme = 'white')
    }
  })
  
  
  #Plot Protfolio time series#############################
  output$weightened.portfolio <- renderPlot({
    # 1 bios 5 tage useless, da daten jenachdem nicht genug abdecken
    # if (input$sliderHistorie=="1D") b <- 1 
    # if (input$sliderHistorie=="5D") b <- 5
    if (input$sliderHistorie=="1M") b <- 30
    if (input$sliderHistorie=="6M") b <- 182
    if (input$sliderHistorie=="1Y") b <- 365
    if (input$sliderHistorie=="5Y") b <- 5*365
    if (input$sliderHistorie=="8Y") b <- 8*365
    dat_mvp_F()
    dat_tp_F()
    dat_mvp_rec_F()
    dat_tp_rec_F()
    inputs_num()
    weightened.portfolio_F(b)
    
    port <- xts()
    port <- merge(port,weightened.portfolio[,6],weightened.portfolio.mvp[,6],weightened.portfolio.tp[,6])
    colnames(port)<- c("Alt", "NeuMVP","NeuTP")
    
    output$mvp.venturini <- renderText({
      paste("MVP = ", round(tail(as.numeric(weightened.portfolio.mvp$Adjusted), n = 1) - head(as.numeric(weightened.portfolio.mvp$Adjusted), n = 1)),".-",sep = " ")
    })
    
    output$alt <- renderText({
      paste("Bestehend = ", round(tail(as.numeric(weightened.portfolio$Adjusted), n = 1) - head(as.numeric(weightened.portfolio$Adjusted), n = 1)),".-",sep = " ")
    })
    
    output$tp.venturini <- renderText({
      paste("TP = ", round(tail(as.numeric(weightened.portfolio.tp$Adjusted), n = 1) - head(as.numeric(weightened.portfolio.tp$Adjusted), n = 1)),".-",sep = " ")
    })
    
    output$individuell <- renderText({
      paste("Individuell = ", round(tail(as.numeric(weightened.portfolio.max$Adjusted), n = 1) - head(as.numeric(weightened.portfolio.max$Adjusted), n = 1)),".-",sep = " ")
    })
    
    if (input$radioHistorie == 1 & b != 1){
      ggplot(data = port, aes(index(port)))+
        ggtitle("Historien der Portfolios")+
        theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 15))+
        geom_line(aes(y = Alt, colour = "Bestehendes")) + 
        geom_line(aes(y = NeuMVP, colour = "Minimum\nVaraince")) +
        geom_line(aes(y = NeuTP, colour = "Tangential")) +
        labs(color = "Portfolios")+
        ylab("Portfoliowert [CHF]") +
        xlab("Zeit [Monate / Jahre]")
      
    }
    else if (input$radioHistorie == 2){
      chartSeries(weightened.portfolio ,name="Historie des bestehenden Portfolios",theme = 'white')
    }
    
  })
  
  output$weightened.portfolio2 <- renderPlot({
    # 1 bios 5 tage useless, da daten jenachdem nicht genug abdecken
    # if (input$sliderHistorie=="1D") b <- 1 
    # if (input$sliderHistorie=="5D") b <- 5
    if (input$sliderHistorie=="1M") b <- 30
    if (input$sliderHistorie=="6M") b <- 180
    if (input$sliderHistorie=="1Y") b <- 365
    if (input$sliderHistorie=="5Y") b <- 5*365
    if (input$sliderHistorie=="8Y") b <- 8*365
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    dat_max_F()
    dat_max_rec_F()
    inputs_num()
    weightened.portfolio2_F(b)
    
    if (input$radioHistorie == 1 & b != 1){
      ggplot(data = weightened.portfolio.max, aes(as.Date(index(weightened.portfolio.max))))+
        geom_line(aes(y = Adjusted, colour = "Individuell"))+
        labs(color = "Portfolio")+
        ylab("Portfoliowert [CHF]") +
        xlab("Zeit [Monate / Jahre]")
    }
    else if (input$radioHistorie == 2){
      chartSeries(weightened.portfolio.max ,name="Historie des individuellen",theme = 'white')
    }
    
  })
  
  output$mvprec <- renderTable({
    inputs_num()
    dat_mvp_rec_F()
    dat_mvp_rec
  })
  
  
  output$tprec <- renderTable({
    inputs_num()
    dat_tp_F(input$shortpara)
    dat_tp_rec_F()
    dat_tp_rec
  })
  
  
  output$maxrec <- renderTable({
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    dat_max_rec_F()
    dat_max_rec
  })
  
  
  output$mvprec_inf <- renderTable({
    inputs_num()
    mvprec_inf <- data.frame("Volatilität"=round(mvpvola,2),
                             "Rendite"=round(mvpreturn,2))
    mvprec_inf
  })
  
  output$tprec_inf <- renderTable({
    inputs_num()
    input$shortpara
    tprec_inf <- data.frame("Volatilität"=round(tpvola,2),
                            "Rendite"=round(tpreturn,2))
    tprec_inf
  })
  
  
  output$maxrec_inf <- renderTable({
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    maxrec_inf <- data.frame("Volatilität"=round(max_vola,2),
                             "Rendite"=round(max_return,2))
    maxrec_inf
  })
  
  output$map <- renderLeaflet({
    data <- data.frame(
      asset = c("Swiss Market Index",
                "S&P 500 Index & Gold",
                "Swiss Government Bond",
                "U.S. Government Bond"),
      lat = c(47.37147, 40.70704, 46.94657, 38.89766),
      lng = c(8.53208, -74.01119, 7.44429, -77.03641),
      desc = c("Der Swiss Market Index (SMI) ist der führende Aktienindex der Schweiz, der die 20 größten und liquidesten Unternehmen des Landes abbildet.",
               "Der S&P 500 ist ein Aktienindex, der die Performance von 500 der größten börsennotierten Unternehmen in den USA widerspiegelt. 
               Gold ist ein Edelmetall, das als Rohstoff für Schmuck, elektronische Bauteile und Investitionen verwendet wird. Es gilt als sicheres Investment.",
               "Eine Schweizer Staatsanleihe ist eine Schuldverschreibung der Schweizer Regierung, mit der sie sich Kapital beschafft und den Gläubigern regelmäßige Zinszahlungen sowie die Rückzahlung des Kapitals zum Fälligkeitsdatum verspricht.",
               "Ein US Government Bond ist eine Anleihe, die von der Regierung der Vereinigten Staaten ausgegeben wird und als Schuldtitel fungiert.")
    )
    
    # Define the borders of Switzerland and the United States
    borders <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
    swiss_border <- borders[borders$name == "Switzerland",]
    us_border <- borders[borders$name == "United States of America",]
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        ~lng, ~lat,
        popup = ~paste("<strong>", asset, "</strong><br>", desc),
        label = ~asset
      ) %>%
      addPolygons(
        data = swiss_border,
        fillColor = "#FF0000",
        fillOpacity = 0.05,
        color = "#FF0000",
        weight = 2
      ) %>%
      addPolygons(
        data = us_border,
        fillColor = "#0000FF",
        fillOpacity = 0.05,
        color = "#0000FF",
        weight = 2
      )
  })
  
  vals <- reactiveValues(p1=NULL)
  output$overview<- renderDataTable({
    risk_F(input_slid3())
    if (risk == 2) zu_invest_verm <<- 2*input$num15
    else zu_invest_verm <<- input$num15
    input_ckbx()
    dat_max_F()
    dat_max_rec_F()
    inputs_num()
    dat_mvp_F()
    dat_tp_F()
    dat_mvp_rec_F()
    dat_tp_rec_F()
    
    
    a <- xts()
    for (i in 1:length(portfolio_s)) {
      if (portfolio_s2[i] > 0)
        a <- na.omit(merge(a, ren[[i]]))
    }
    y <-window(a, start=Sys.Date()-365*2, end=Sys.Date())
    ret_b <- sum(colMeans(y)*100)
    sd_b <- sqrt(sum(colSds(y)))
    n <- sum(portfolio_s)#length(portfolio_s[portfolio_s == 0])
    
    ov_basic <- c(portfolio_s[portfolio_s != 0]/n,ret_b,sd_b,(ret_b-riskfree)/sd_b)
    ov_mvp <- c(dat_mvp$Gewicht,mvpreturn,mvpvola,(mvpreturn-riskfree)/mvpvola)
    ov_tp <- c(dat_tp$Gewicht,tpreturn,tpvola,(tpreturn-riskfree)/tpvola)
    ov_max <- c(dat_max$Gewicht,max_return,max_vola,(max_return-riskfree)/max_vola)
    
    overview_df <- data.frame("Basic"=ov_basic,"MVP"=ov_mvp,"TP"=ov_tp)
    row.names(overview_df) <- c(dat_mvp$Asset,"Rendite","Vola","Sharp")
    overview_df <- round(overview_df,2)
    vals$p1 <- tableGrob(overview_df)
    datatable(round(overview_df,2), options = list(dom = 't'))
  })
  
  
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("portfolio_performance_FusionFinance_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file,onefile=T)
      grid.arrange(vals$p1)
      dev.off()
    },
    contentType = "application/pdf"
  )
  
  
  
  ####################################Help-Box##################################
  help_text <- reactive({
    if (input$help_tab1) whichtab <- "help_tab1"
    if (input$help_tab2) whichtab <- "help_tab2"
    if (input$help_tab3) whichtab <- "help_tab3"
    if (input$help_tab4) whichtab <- "help_tab4"
    if (input$help_tab5) whichtab <- "help_tab5"
    if (input$help_tab6) whichtab <- "help_tab6"
    if (input$help_tab7) whichtab <- "help_tab7"
    if (input$help_tab8) whichtab <- "help_tab8"
    subset(helptext, tab == whichtab)
  })
  
  observeEvent(input$help_tab1,
               introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab2,
               introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab3,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab4,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab5,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab6,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab7,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab8,
               introjs(session, options = list("showBullets"="false", "showProgress"="true",
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  
  
  helptext <- data.frame(
    tab = c("help_tab1", "help_tab1", "help_tab1", "help_tab2","help_tab2","help_tab2", "help_tab3", "help_tab4","help_tab4","help_tab4", "help_tab4", 
            "help_tab5", "help_tab5", "help_tab5", "help_tab5", "help_tab5", "help_tab6", "help_tab6", "help_tab6", "help_tab6", "help_tab7", "help_tab7", "help_tab7", "help_tab8", "help_tab8")
    , step <- c(3, 3, 3,    3, 3, 3,    1,    4,4,4,4,    5,5,5,5,5,    4,4,4,4,   3,3,3,  2,2)
    , element = c("#num1", "#portfolio_worth1", "#portfolio1", 
                  "#num15","#Risikobereitschaft","#checkbox1", 
                  "#radioHistorie", 
                  "#mvprec ","#mvp", "#mvp2","#mvprec_inf",
                  "#tprec", "#shortpara", "#tp", "#tp2", "#tprec_inf",
                  "#maxrec", "#max", "#max2", "#maxrec_inf",
                  "#selected_var", "#radio1", "#historical_data",
                  "#overview", "#download_pdf")
    , intro = c("Wählen Sie hier die Anzahl Ihrer Assets","Hier sehen Sie den Wert Ihres Portfolios","Hier ist die Verteilung der Assets im Portfolios dargestellt",
                "Geben Sie Ihr zu investierendes Vermögen ein","Geben Sie hier Ihre Risikobereitschaft ein","Wählen Sie hier die Assets, die in Ihrem Portfolio haben möchten",
                "Hier kann zwischen der Standardansicht und einer erweiterten Ansicht gewechselt werden", 
                "Hier werden die Gewichte der verwendetes Assets des Portfolios numerisch dargestellt","Hier ist die Verteilung der Assets im Portfolio grafisch als Kuchendiagramm dargestellt", "Hier werden die Gewichte der Assets zusätzlich als Balkendiagramm dargestellt","Hier sind die beiden Performance Indikatoren Volatiliät und Rendite des Portfolios in Dezimalschreibweise aufgeführt",
                "Hier werden die Gewichte der verwendetes Assets des Portfolios numerisch dargestellt", "Im Vergleich zum MVP-Portfolio ist Shorting hier erlaubt und kann über diesen Knopf aktiviert/deaktiviert werden","Hier ist die Verteilung der Assets im Portfolio grafisch als Kuchendiagramm dargestellt", "Hier werden die Gewichte der Assets zusätzlich als Balkendiagramm dargestellt (Rot bedeutet ein negatives Gewicht, also dieses ASset wird geshortet)","Hier sind die beiden Performance Indikatoren Volatiliät und Rendite des Portfolios in Dezimalschreibweise aufgeführt",
                "Hier werden die Gewichte der verwendetes Assets des Portfolios numerisch dargestellt","Hier ist die Verteilung der Assets im Portfolio grafisch als Kuchendiagramm dargestellt", "Hier werden die Gewichte der Assets zusätzlich als Balkendiagramm dargestellt (Rot bedeutet ein negatives Gewicht, also dieses Asset wird geshortet)","Hier sind die beiden Performance Indikatoren Volatiliät und Rendite des Portfolios in Dezimalschreibweise aufgeführt",
                "Im obigen Drop-down Menu können Sie ein Asset auswählen, von welchem Sie den Kursverlauf anschauen möchten", "Hier kann zwischen der Standardansicht und einer erweiterten Ansicht gewechselt werden", "Dieses Liniendiagramm zeigt den Kursverlauf über die angewählte Zeitspanne an",
                "Hier ist eine Übericht mit aller Performance- Indikatoren und Gewichten sämtlicher zusammengestellten Portfolios", "Drücken Sie hier um die Übersichtstabelle als PDF herunterzuladen")
  )
}