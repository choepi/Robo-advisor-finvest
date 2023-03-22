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

## build ui.R -----------------------------------
## 1. header -------------------------------
ui <- fluidPage(
  header <- dashboardHeader(title=div(img(src="fusion.jpg", height=40)
  )),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Profil", tabName = "profil", icon = icon("user")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("folder-open")),
      menuItem("Kurse", tabName = "kurse", icon = icon("eye")))),
  
  body <- dashboardBody( tabItems(
    tabItem(tabName = "profil",
            h1("Profil"),
            h5("Hier können sie ihr Portfolio eingeben oder erstellen lassen"),
            mainPanel(tabsetPanel(
              id = "tabsetPanelID",
              type = "tabs",
              tabPanel("Bestehendes Portfolio"),
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
              tabPanel("MVP"),
              tabPanel("Tangential")
            ))
    ),
    tabItem(tabName = "kurse",
            h1("Kurse"),
            h5("Aktuelle Kursangaben"),
            fluidRow(
              column(9,
                     dateInput("dateinput1", 
                               h3("Auswahl Historie bis"), 
                               value = "2020-01-01"),
                     br(), 
                     selectInput("select2", h3("SMI"),
                                 choices = list("SMI" ="SMI","SWIBND" = "SWIBND",
                                                "GOLD"="GOLD","BITCOIN"="BITCOIN",
                                                "SNP500"="SNP500","USBND"="USBND",
                                                "USDCHF"="USDCHF"),selected = "SMI"),
                     br(), 
                     submitButton("Submit")
              ),
              column(9,
                     textOutput("selected_var"),
                     plotOutput("historical_data")
              )),
            
    )
  )
  ),
  
)