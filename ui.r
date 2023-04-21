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

## build ui.R -----------------------------------
## 1. header -------------------------------
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
      menuItem("Kurse", tabName = "kurse", icon = icon("eye")),
      menuItem("Über uns", tabName = "about", icon = icon("people-group")))),
  
  body <- dashboardBody( 
    introjsUI(),
    tabItems(
      tabItem(tabName = "profil",
              h1("Profil"),
              mainPanel(tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Bestehendes Portfolio",
                         actionButton("help1", "About this Page"),
                         h4("Hier kannst du dein bestehendes Portfolio eintragen, damit du später in der Maske 
                            Portfolio mit dem MVP oder dem Tangentialportfolio vergleichen kannst.
                            Selbstverständlich dürfen Sie auch willkürliche Gewichtungen eintragen, um ein Gefühl für
                            verschieden Assets zu erhalten."),
                         fluidRow(
                           
                           column(2,
                                  introBox(
                                    numericInput("num1", label = h5("SMI"), value = 1, width = 100, min = 0),

                                    introBox(
                                      numericInput("num2", label = h5("SWIBND"), value = 0, width = 100, min = 0),
                                      data.step = 1,
                                      data.intro = "Auswahl welches zu Ihnen passt"))),
                           column(2,
                                  numericInput("num3", label = h5("GOLD"), value = 1, width = 100, min = 0),
                                  numericInput("num4", label = h5("BITCOIN"), value = 0, width = 100, min = 0)),
                           column(2,
                                  numericInput("num5", label = h5("SNP500"), value = 0, width = 100, min = 0),
                                  numericInput("num6", label = h5("USBND"), value = 0, width = 100, min = 0)),
                           column(2,
                                  numericInput("num7", label = h5("USDCHF"), value = 0, width = 100, min = 0)),
                           
                           mainPanel(
                             introBox(
                               h4(textOutput("portfolio_worth1")),
                               data.step = 2,
                               data.intro = "Hier sehen sie den aktuellen Wert ihres Portfolios"),
                             plotOutput("portfolio1", width = "100%")),
                         )),
                tabPanel("Kein Portfolio",
                         actionButton("help2", "About this Page"),
                         fluidPage(
                           h4("Hier kannst du mittels deinen individuellen Wünschen eine Portfoliooempfehlung erhalten,
                            welches du dann nach bedarf anpassen kannst."),
                           br(),
                           basicPage(
                             h4("Zu investierendes Vermögen:"),
                             introBox(
                               numericInput("num15", label = h6(""), value = 0, width = 100, min = 0),
                               data.step = 1,
                               data.intro = "Eingabe"),
                             sliderTextInput(
                               inputId = "slider3",
                               label = "Risikobereitschaft",
                               choices = c("Geringes Risiko", "Mittleres Risiko", "Hohes Risiko"),
                               selected = "Geringes Risiko"
                             ),
                             column(5,
                                    checkboxInput("checkbox1", "SMI", value = F),
                                    checkboxInput("checkbox2", "SWIBND", value = F),
                                    checkboxInput("checkbox3", "GOLD", value = F),
                                    checkboxInput("checkbox4", "BITCOIN", value = F)),
                             column(5,
                                    checkboxInput("checkbox5", "SNP500", value = F),
                                    checkboxInput("checkbox6", "USBND", value = F),
                                    checkboxInput("checkbox7", "USD", value = F))),
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
                         sliderTextInput(
                           inputId = "sliderHistorie",
                           label = "Zeitraum",
                           choices = c("1D","5D","1M","6M","1Y","5Y","Max."),
                           selected = "5D"),
                         br(),
                         radioButtons("radioHistorie", h3("Ansicht"),
                                      choices = list("Simpel" = 1, "Erweitert" = 2),
                                      selected = 2),
                         br(), 
                         plotOutput("weighted.portfolio",width = "60%")
                ),
                tabPanel("MVP",
                         fluidRow(
                           tableOutput("mvprec"),
                           plotOutput("mvp",width = "60%"))),
                tabPanel("TP",
                         fluidRow(
                           tableOutput("tprec"),
                           plotOutput("tp", width = "60%")))
              ))
      ),
      tabItem(tabName = "kurse",
              h1("Kurse"),
              h5("Aktuelle Kursangaben"),
              fluidPage(
                sidebarPanel(
                  sliderTextInput(
                    inputId = "slider2",
                    label = "Choice",
                    choices = c("1D","5D","1M","6M","1Y","5Y","Max."),
                    selected = "5D"
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
                               selected = 2)
                  ,width = 2),
                mainPanel(
                  plotOutput("historical_data", width = "60%")
                )),
      ),
      tabItem(tabName = "about",
              includeHTML("about.html"))
    )
  ),
)
dashboardPage(header, sidebar, body,skin = "black")
