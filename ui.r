library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs) 
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
library(timevis)
library(shinyWidgets)
library(rintrojs)
library(xml2)
library(rvest)
library(leaflet)
library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(expm)
library(imputeTS)
library(DEoptim)
library(shinyjs)
library(scales)
library(pROC)
library(fPortfolio)
library(PortfolioAnalytics)
library(tidyquant)
library(geojsonio)
library(gridExtra)


## build ui.R -----------------------------------
## 1. header -------------------------------
ui <- dashboardPage(
  header <- dashboardHeader(title=div(img(src="fusion.jpg", height=120),
                                      tags$style(".navbar {min-height: 80px !important;}"),
                                      tags$style(".main-header {min-height: 80px !important;}"),
                                      tags$style(".logo img {height: 80px !important;}"),
                                      tags$li(
                                        class = "dropdown",
                                        tags$style(".main-header .logo {height: 80px !important;}"),
                                        tags$style(".sidebar-toggle {font-size: 30px !important;}"),
                                        tags$style(".dropdown-toggle {font-size: 35px !important;}"))
                                      
  ),
  
  dropdownMenu(icon = icon("circle-info"),  messageItem(
    from = "",
    icon = icon("headset"),
    message = (img(src="support.jpg", height=180)),
    tags$li(
      tags$head(
        class = "dropdown",
        tags$style(".dropdown-menu .fa-circle-info { font-size: 100px !important; }")))
  ))
  ),
  sidebar <- dashboardSidebar(
    tags$head(tags$style(".sidebar { margin-top: 40px; }")),
    sidebarMenu(
      menuItem("Profil", tabName = "profil", icon = icon("user")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("folder-open")),
      menuItem("Kurse", tabName = "kurse", icon = icon("eye")),
      menuItem("Mehr Infos", tabName = "AssetInfo", icon = icon("folder-open")),
      menuItem("Über uns", tabName = "about", icon = icon("people-group")),
      tags$style(".sidebar-menu li a {font-size: 18px;}"))),
  
  body <- dashboardBody( 
    introjsUI(),
    tabItems(
      tabItem(tabName = "profil",
              h1("Profil"),
              tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Bestehendes Portfolio",
                         actionButton("help_tab1", "Hilfe"),
                         h4("Hier können Sie ihr bestehendes Portfolio eintragen, damit Sie dieses später in der Maske 
                            Portfolio mit dem Minimum" , br(), "Varianz Portflio oder dem Tangentialportfolio vergleichen können.
                            Selbstverständlich dürfen Sie auch willkürliche" , br(), "Gewichtungen eintragen, um ein Gefühl für
                            verschieden Assets zu erhalten."),
                         fluidRow(
                           
                           column(2,
                                  numericInput("num1", label = h5("SMI"), value = 1, width = 100, min = 0),
                                  textOutput("Asset1"),
                                  br(),
                                  br(),
                                  numericInput("num2", label = h5("SWIBND"), value = 0, width = 100, min = 0),
                                  textOutput("Asset2")
                           ),
                           column(2,
                                  numericInput("num3", label = h5("GOLD"), value = 1, width = 100, min = 0),
                                  textOutput("Asset3"),
                                  br(),
                                  br(),
                                  numericInput("num4", label = h5("BITCOIN"), value = 0, width = 100, min = 0),
                                  textOutput("Asset4")
                           ),
                           column(2,
                                  numericInput("num5", label = h5("SNP500"), value = 1, width = 100, min = 0),
                                  textOutput("Asset5"),
                                  br(),
                                  br(),
                                  numericInput("num6", label = h5("USBND"), value = 0, width = 100, min = 0),
                                  textOutput("Asset6")
                           ),
                           mainPanel(
                             h4(textOutput("portfolio_worth1")),
                             plotOutput("portfolio1", width = "70%")
                           ),
                         )
                ),
                tabPanel("Portfolio erstellen",
                         actionButton("help_tab2", "Hilfe"),
                         fluidPage(
                           h4("Hier erhalten Sie mittels ihren individuellen Wünschen eine Portfolioempfehlung,
                            welche Sie dann nach Bedarf anpassen können." , br(), "Hierbei informieren Sie uns über ihren gewünschten 
                            zu investierenden Betrag und ihre Risikobereitschaft."),
                           br(),
                           basicPage(
                             h4("Zu investierendes Vermögen:"),
                             introBox(
                               numericInput("num15", label = h6(""), value = 1000, width = 100, min = 0),
                               data.step = 1,
                               data.intro = "Eingabe"),
                             sliderTextInput(
                               inputId = "slider3",
                               label = "Risikobereitschaft",
                               choices = c("Geringes Risiko", "Mittleres Risiko", "Hohes Risiko"),
                               selected = "Mittleres Risiko"
                             ),
                             column(5,
                                    checkboxInput("checkbox1", "SMI", value = T),
                                    checkboxInput("checkbox2", "SWIBND", value = T),
                                    checkboxInput("checkbox3", "GOLD", value = T),
                                    checkboxInput("checkbox4", "BITCOIN", value = T)),
                             column(5,
                                    checkboxInput("checkbox5", "SNP500", value = T),
                                    checkboxInput("checkbox6", "USBND", value = T))),
                         )),
              )
      ),
      
      tabItem(tabName = "portfolio",
              h1("Portfolio"),
              h4("Einsehbarkeit der Performance des Portfolios anhand der Minimum Varianz oder der Tangential Methode"),
              tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Übersicht",
                         actionButton("help_tab8", "Hilfe"),
                         h4("Hier sehen Sie eine Übersicht über die verschiedenen Gewichtungen ihres aktuellen 
                            Porfolios, des Minimum" , br(), "Varianz Portfolios und des Tangentialportfolios.
                            Gerne können Sie die Daten mittels einem Klick auf den Button " , br(), "\"Download PDF\" herunterladen."),
                         fluidPage(fluidRow(
                           downloadButton("download_pdf", "Download PDF"),
                           column(6,
                                  dataTableOutput("overview"))))),
                tabPanel("Historie",
                         actionButton("help_tab3", "Hilfe"),
                         h4("Hier können Sie ihr bestehendes Portfolio mit dem Minimum Varianz Portfolio und dem 
                            Tangentialportfolio vergleichen.", br(), "Dabei können Sie einen gewünschten Zeitraum und die Ansicht 
                            je nach Bedarf anpassen."),
                         fluidPage(fixedPage(position = "right", sidebarPanel(
                           sliderTextInput(
                             inputId = "sliderHistorie",
                             label = h3("Zeitraum"),
                             choices = c("1M","6M","1Y","5Y","8Y"),
                             selected = "1M"),
                           br(),
                           radioButtons("radioHistorie", h3("Ansicht"),
                                        choices = list("Simpel" = 1, "Erweitert" = 2),
                                        selected = 1),
                           br(), 
                           h3("Renditen [CHF]"),
                           textOutput("alt"),
                           textOutput("mvp.venturini"),
                           textOutput("tp.venturini"),
                           textOutput("individuell"),
                           width = 2),
                           mainPanel(
                             plotOutput("weightened.portfolio"),
                             plotOutput("weightened.portfolio2")
                           )
                         )
                         )
                ),
                tabPanel("MVP",
                         actionButton("help_tab4", "Hilfe"),
                         h4("Hier können Sie sehen, welche Handlungen ihrerseits getätigt werden müssten, um das Minimum Varianz Portfolio zu erhalten."),
                         h4("Das Minimum Varianz Portfolio ist eine Portfoliokonstruktionstechnik, bei der Assets ausgewählt werden, um das 
                            Portfolio", br(), "mit der geringstmöglichen Volatilität oder dem geringsten Risiko zu erstellen, unabhängig von der erwarteten 
                            Rendite.", br(), "Es wird eine optimale Balance zwischen den verschiedenen Vermögenswerten gefunden, um das Portfoliorisiko zu minimieren."),
                         fluidRow(
                           tableOutput("mvprec"),
                           splitLayout(cellWidths = c("40%", "40%"), plotOutput("mvp"), plotOutput("mvp2")),
                           tableOutput("mvprec_inf"))),
                tabPanel("TP",
                         actionButton("help_tab5", "Hilfe"),
                         h4("Hier können Sie sehen, welche Handlungen ihrerseits getätigt werden müssten, um das Tangentialportfolio zu erhalten."),
                         h4("Das Tangentialportfolio ist ein Portfolio, das auf der effizienten Grenze liegt und die maximale Rendite für ein gegebenes", br(), "Risikoniveau bietet. 
                            Es ist die optimale Kombination von risikoreichen Anlagen, um das Renditepotenzial zu maximieren,", br(), "während das Risiko kontrolliert wird."),
                         fluidRow(
                           tableOutput("tprec"),
                           checkboxInput("shortpara", "Shorten erlaubt",value = F),
                           splitLayout(cellWidths = c("40%", "40%"), plotOutput("tp"), plotOutput("tp2")),
                           tableOutput("tprec_inf"))),
                tabPanel("erstelltes Portfolio",
                         actionButton("help_tab6", "Hilfe"),
                         h4("In dieser Maske sehen Sie unsere Kaufempfehlung basierend auf Ihren persönlichen Angaben aus der Maske Profil \"Portfolio erstellen\"."),
                         fluidRow(
                           tableOutput("maxrec"),
                           splitLayout(cellWidths = c("40%", "40%"), plotOutput("max"), plotOutput("max2")),
                           tableOutput("maxrec_inf")))
              )
      ),
      tabItem(tabName = "kurse",
              h1("Kurse"),
              actionButton("help_tab7", "Hilfe"),
              h4("Hier können Sie sich den Verlauf der Kurse der verschiedenen Assets genauer ansehen.", br(), "
                 Dabei können Sie die Laufzeit, den gewünschten Asset und die Ansicht selbst anpassen."),
              fluidPage(fixedPage(position = "right",
                                  sidebarPanel(
                                    sliderTextInput(
                                      inputId = "slider2",
                                      label = "Laufzeit",
                                      choices = c("1D","5D","1M","6M","1Y","5Y","Max."),
                                      selected = "6M"
                                    ),
                                    br(), 
                                    selectInput("select2", h3("Asset"),
                                                choices = list("BITCOIN"=4,"SMI" =1,"SWIBND" = 2,
                                                               "GOLD"=3,
                                                               "SNP500"=5,"USBND"=6,
                                                               "USD"=7),selected = "BITCOIN"),
                                    textOutput("selected_var"),
                                    br(), 
                                    radioButtons("radio1", h3("Ansicht"),
                                                 choices = list("Simpel" = 1, "Erweitert" = 2),
                                                 selected = 1)
                                    ,width = 2),
                                  mainPanel(
                                    plotOutput("historical_data", width = "100%")
                                  )
              )
              ),
      ),
      tabItem(tabName = "about",
              includeHTML("about.html")),
      
      tabItem(tabName = "AssetInfo",
              tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("AssetInfo",
                         includeHTML("AssetInfo.html")),
                tabPanel("Geografische Verteilung",
                         tabPanel("Map", leafletOutput("map", width = "1250px", height = "1000px"))
                ),
                tabPanel("Portfoliotheorie",
                         includeHTML("Portfoliotheorie.html"))
              )
      )
    )
  ),
)
dashboardPage(header, sidebar, body,skin = "black")
