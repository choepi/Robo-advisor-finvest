library(shiny)

# Load functions ----
source("global.r")

# UI ----
source("ui.r")

# Function and Calculation ----
source("server.r")

shinyApp(ui = ui, server = server)
