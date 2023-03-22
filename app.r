library(shiny)

# Load functions ----
source("global.r")

# UI ----
source("ui.R")

# Function and Calculation ----
source("server.R")

shinyApp(ui = ui, server = server)