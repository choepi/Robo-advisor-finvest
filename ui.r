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
<<<<<<< HEAD
=======

>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c

## build ui.R -----------------------------------
## 1. header -------------------------------
ui <- dashboardPage(
  header <- dashboardHeader(title=div(img(src="fusion.jpg", height=60)
  ),
  dropdownMenu(icon = icon("circle-info"),  messageItem(
    from = "",
    icon = icon("headset"),
    message = (img(src="support2.jpg", height=180))
  ))
  ),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Profil", tabName = "profil", icon = icon("user")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("folder-open")),
      menuItem("Kurse", tabName = "kurse", icon = icon("eye")))),
  
  body <- dashboardBody( 
    tabItems(
      tabItem(tabName = "profil",
              h1("Profil"),
              h5("Hier können sie ihr Portfolio eingeben oder erstellen lassen"),
              mainPanel(tabsetPanel(
                id = "tabsetPanelID",
                type = "tabs",
                tabPanel("Bestehendes Portfolio",
                         column(2,
                         numericInput("num1", label = h5("SMI"), value = 1, width = 100),
                         numericInput("num2", label = h5("SWIBND"), value = 1, width = 100),
                         numericInput("num3", label = h5("GOLD"), value = 1, width = 100),
                         numericInput("num4", label = h5("BITCOIN"), value = 1, width = 100)),
                         column(2,
                         numericInput("num5", label = h5("SNP500"), value = 1, width = 100),
                         numericInput("num6", label = h5("USBND"), value = 1, width = 100),
                         numericInput("num7", label = h5("USDCHF"), value = 1, width = 100)),
                         column(2,
                         )
                         ),
                tabPanel("Kein Portfolio"),
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
                         fluidRow(
                           plotOutput("mvp"))),
                tabPanel("Tangential")
              ))
      ),
      tabItem(tabName = "kurse",
              h1("Kurse"),
              h5("Aktuelle Kursangaben"),
              fluidRow(
                column(9,
                       #sliderInput("slider2", h3("Zeit Horizont"),
<<<<<<< HEAD
                       #           min = 1, max = 7, value = 4),
=======
                        #           min = 1, max = 7, value = 4),
>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c
                       #helpText("1D  5D   1M   6M  1Y   5J  Max."),
                       sliderTextInput(
                         inputId = "slider2",
                         label = "Choice",
                         choices = c("1D","5D","1M","6M","1Y","5Y","Max."),
                         selected = "1D"
                       ),
                       br(), 
                       selectInput("select2", h3("SMI"),
<<<<<<< HEAD
                                   choices = list("SMI" ="SMI","SWIBND" = "SWIBND",
                                                  "GOLD"="GOLD","BITCOIN"="BITCOIN",
                                                  "SNP500"="SNP500","USBND"="USBND",
                                                  "USDCHF"="USDCHF"),selected = "SMI"),
                       
=======
                                   choices = list("SMI" =1,"SWIBND" = 2,
                                                  "GOLD"=3,"BITCOIN"=4,
                                                  "SNP500"=5,"USBND"=6,
                                                  "USDCHF"=7),selected = "SMI"),

>>>>>>> 8e6addcd611c4b6204bec69f184911904298934c
                       br(), 
                       radioButtons("radio1", h3("Ansicht"),
                                    choices = list("Simpel" = 1, "Erweitert" = 2),
                                    selected = 1),
                ),
                column(9,
                       textOutput("selected_var"),
                       plotOutput("historical_data")
                )),
              
      )
    )
  ),
  
)
dashboardPage(header, sidebar, body,skin = "black")
