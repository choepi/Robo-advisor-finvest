library(shiny)
library(quantmod)
library(rsconnect)
rsconnect::setAccountInfo(name='fusionfinance',
                          token='7F40E0665C6370082775CB421F2570BF',
                          secret='qieZeYAIhQpl8uE8+zwjI78xAoDQdXEMIwjWoBsI')

#----
ui <- fluidPage(
  titlePanel("Markowitz Portfolio Optimization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("yield", "Expected annual yield (%)", 10, min = 0, max = 100),
      numericInput("assets", "Number of assets", 5, min = 1, max = 100),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
server <- function(input, output) {
  
  # Function to calculate portfolio with lowest risk using Markowitz approach
  calc_portfolio <- function(yield, assets) {
    
    # Get stock data for selected assets
    tickers <- toupper(paste0("stock", 1:assets))
    stock_data <- getSymbols(tickers, from = "2018-01-01", to = Sys.Date(), auto.assign = FALSE)
    returns <- do.call(merge, lapply(stock_data, function(x) dailyReturn(Cl(x))))
    colnames(returns) <- tickers
    
    # Calculate expected returns and covariance matrix
    exp_returns <- mean(returns) * 252
    cov_matrix <- cov(returns) * 252
    
    # Calculate optimal portfolio using Markowitz approach
    portfolio <- portfolio.optim(cov.matrix = cov_matrix, mu = exp_returns, target.return = yield/100, short = FALSE)
    weights <- extractWeights(portfolio)
    return(weights)
  }
  
  # Observe the "Calculate" button and execute the calc_portfolio function
  observeEvent(input$calculate, {
    output$plot <- renderPlot({
      weights <- calc_portfolio(input$yield, input$assets)
      barplot(weights, main = "Optimal Portfolio Weights", xlab = "Asset", ylab = "Weight", ylim = c(0, 1))
    })
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

