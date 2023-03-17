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
  titlePanel(title=div(img(src="fusion.jpg", height=100)
                       ,"Portfolio Optimizing Tool")),
  sidebarLayout(
    sidebarPanel(h1("Willkommen"),
                 fluidRow(
                   column(7,
                          selectInput("select1", h3("Menü"),
                                       choices = list("Home" = 1, "Optimierung" = 2,
                                                      "Forecast" = 3),selected = 1),
                          br(),
                          br(), 
                          actionButton("action1", "Help"),
                          ))
                 ),
    mainPanel(
      h1("Patriks branch"),
      h2("Test"),
      fluidRow(
        column(9,
               dateInput("dateinput1", 
                         h3("Auswahl Historie bis"), 
                         value = "2013-01-01"),
               br(),
               br(), 
               sliderInput("slider1", h3("Risikolevel %"),
                           min = 0, max = 100, value = 50),
               br(),
               br(), 
               submitButton("Submit")
        )),
      textOutput("selected_var")
    )
  )
)
