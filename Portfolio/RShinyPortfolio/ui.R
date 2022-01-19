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
      uiOutput("slider") 
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
