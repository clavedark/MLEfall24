#shiny app to demonstrate the effects of intercept shifts and multiplier changes on the cumulative distribution function (CDF) of different distributions; coauthored w/ claude.ai sept 2024


library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Interactions and Intercept Shifts in Nonlinear Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Choose Distribution:",
                  choices = c("Logistic", "Standard Normal", "Complementary Log-Log", "Cauchy")),
      sliderInput("add_const", "Intercept Shift:",
                  min = -3, max = 3, value = 0, step = 0.1),
      sliderInput("mult_const", "Multiplier:",
                  min = 0.1, max = 3, value = 1, step = 0.1)
    ),
    mainPanel(
      plotOutput("cdfPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to calculate CDF based on chosen distribution
  calculate_cdf <- function(x, dist, location = 0, scale = 1) {
    switch(dist,
           "Logistic" = 1 / (1 + exp(-(x - location) / scale)),
           "Standard Normal" = pnorm(x, mean = location, sd = scale),
           "Complementary Log-Log" = 1 - exp(-exp((x - location) / scale)),
           "Cauchy" = pt((x - location) / scale, df = 1)
    )
  }
  
  # Generate plot
  output$cdfPlot <- renderPlot({
    x <- seq(-6, 6, by = 0.1)
    df <- data.frame(x = x)
    
    df <- df %>%
      mutate(
        original = calculate_cdf(x, input$distribution),
        transformed = calculate_cdf((x * input$mult_const) + input$add_const, input$distribution)
      )
    
    ggplot(df, aes(x = x)) +
      geom_line(aes(y = original), color = "lightgrey", size = 1) +
      geom_line(aes(y = transformed), color = "blue", size = 1) +
      labs(title = paste("CDF Transformation:", input$distribution),
           subtitle = paste("Add:", input$add_const, "Multiply:", input$mult_const),
           x = "x",
           y = "Cumulative Probability") +
      theme_minimal() +
      coord_cartesian(ylim = c(0, 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
