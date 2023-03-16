library(shiny)

# Load functions ----
source("global.R")

# UI ----
source("ui.R")

# Function and Calculation ----
source("server.R")

shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='fusionfinance',
                          token='7F40E0665C6370082775CB421F2570BF',
                          secret='<SECRET>')
