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
library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(expm)
library(imputeTS)

ui <- dashboardPage(
  header <- dashboardHeader(title=div(img(src="fusion.jpg", height=60)
  ),
  
  dropdownMenu(icon = icon("circle-info"),  messageItem(
    from = "",
    icon = icon("headset"),
    message = (img(src="support.jpg", height=180))
  ))
  ),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Profil", tabName = "profil", icon = icon("user")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("folder-open")),
      menuItem("Kurse", tabName = "kurse", icon = icon("eye")))),
  
  body <- dashboardBody( 
    introjsUI(),
    tabItems(
      tabItem(tabName = "profil",
              h1("Profil"),
              mainPanel(tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Bestehendes Portfolio",
                         fluidPage(
                         actionButton("help", "About this Page"),
                         h4("Hier kannst du dein bestehendes Portfolio eintragen, damit du später in der Maske 
                            Portfolio mit dem MVP oder dem Tangentialportfolio vergleichen kannst.
                            Selbstverständlich dürfen Sie auch willkürliche Gewichtungen eintragen, um ein Gefühl für
                            verschieden Assets zu erhalten."),
                         introBox(
                           sidebarPanel(
                                  numericInput("num1", label = h5("SMI"), value = 1, width = 100, min = 0),
                                  numericInput("num2", label = h5("SWIBND"), value = 0, width = 100, min = 0),
                                  numericInput("num3", label = h5("GOLD"), value = 1, width = 100, min = 0),
                                  numericInput("num4", label = h5("BITCOIN"), value = 0, width = 100, min = 0),
                                  numericInput("num5", label = h5("SNP500"), value = 0, width = 100, min = 0),
                                  numericInput("num6", label = h5("USBND"), value = 0, width = 100, min = 0),
                                  numericInput("num7", label = h5("USDCHF"), value = 0, width = 100, min = 0)),
                           data.step = 1,data.intro = "Auswahl welches zu Ihnen passt"),
                         mainPanel(
                                plotOutput("portfolio1"),
                                introBox(h4(textOutput("portfolio_worth1")),data.step = 2,
                                         data.intro = "Hier sehen sie den aktuellen Wert ihres Portfolios")),
                )),
                tabPanel("Kein Portfolio",
                         fluidPage(
                         h4("Hier kannst du mittels deinen individuellen Wünschen eine Portfoliooempfehlung erhalten,
                            welches du dann nach bedarf anpassen kannst."),
                         br(),
                         h4("Zu investierendes Vermögen:"),
                         numericInput("num15", label = h6(""), value = 0, width = 100, min = 0),
                         sliderTextInput(
                           inputId = "slider3",
                           label = "Risikobereitschaft",
                           choices = c("Geringes Risiko", "Mittleres Risiko", "Hohes Risiko"),
                           selected = "Geringes Risiko"
                         ),
                         sidebarPanel(
                         ),
                         mainPanel(
                           numericInput("num8", label = h5("SMI"), value = 1, width = 100, min = 0),
                           numericInput("num9", label = h5("SWIBND"), value = 1, width = 100, min = 0),
                           numericInput("num10", label = h5("GOLD"), value = 0, width = 100, min = 0),
                           numericInput("num11", label = h5("BITCOIN"), value = 0, width = 100, min = 0),
                           numericInput("num12", label = h5("SNP500"), value = 0, width = 100, min = 0),
                           numericInput("num13", label = h5("USBND"), value = 0, width = 100, min = 0),
                           numericInput("num14", label = h5("USD"), value = 0, width = 100, min = 0)),
                                
                         )),
                
              ))
      ),
      tabItem(tabName = "portfolio",
              h1("Portfolio"),
              h5("Einsehbarkeit der Performance des Portfolios anhand mvp oder tangential Methode"),
              mainPanel(tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Historie",
                         sliderInput("slider1", h3("Risikolevel %"),
                                     min = 0, max = 100, value = 50)
                ),
                tabPanel("MVP",
                         fluidRow(mainPanel(
                                         plotOutput("mvp")))),
                tabPanel("Tangential",
                         fluidRow(mainPanel(
                           plotOutput("tp"))))
              ))
      ),
      tabItem(tabName = "kurse",
              h1("Kurse"),
              h5("Aktuelle Kursangaben"),
              fluidPage(

                sidebarPanel(
                       #sliderInput("slider2", h3("Zeit Horizont"),
                       #           min = 1, max = 7, value = 4),
                       #helpText("1D  5D   1M   6M  1Y   5J  Max."),
                       sliderTextInput(
                         inputId = "slider2",
                         label = "Choice",
                         choices = c("1D","5D","1M","6M","1Y","5Y","Max."),
                         selected = "1D"
                       ),
                       br(), 
                       selectInput("select2", h3("SMI"),
                                   choices = list("SMI" =1,"SWIBND" = 2,
                                                  "GOLD"=3,"BITCOIN"=4,
                                                  "SNP500"=5,"USBND"=6,
                                                  "USD"=7),selected = "SMI"),
                       textOutput("selected_var"),
                       br(), 
                       radioButtons("radio1", h3("Ansicht"),
                                    choices = list("Simpel" = 1, "Erweitert" = 2),
                                    selected = 1)
                       ),
                mainPanel(
                       plotOutput("historical_data")
                )),
              
      )
    )
  ),
  
)
dashboardPage(header, sidebar, body,skin = "black")