#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Your Optimal Portfolio is only several million random simulations away..."),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Desired_Returns",
                  label = "Specify your risk tolerance with the slider. If you are very risk averse, select 1",
                  min = 1,
                  max = 37,
                  value = 1,
                  step = seq(1, 37, 1),
                  ticks = TRUE,
      )
    ),
    
    #Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotlyOutput("UserweightsPlot")
      ),
      fluidRow(
        plotOutput("PortfolioGrowth")
      ),
      fluidRow(
        plotlyOutput("EfficientFrontier")
      )
    )
  )
))
