#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(zoo)
library(xts)
library(quantmod)
library(tidyverse)
library(shiny)
library(DT)
library(PerformanceAnalytics)
library(tidyquant)
library(timetk)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ggplot2)
library(plotly)

  
  #pull tickers into a tibble compute log daily returns with adjusted column
  stocks_simple_portfolio <- c("spy", "rsp", "ijh", "ijr", "vnq", "tip", 
                               "vwehx", "lqd", "vwo", "vea", "gld") %>% 
    tq_get(get = "stock.prices",
           from = "2007-08-01",
           to = "2021-12-31") %>% 
    group_by(symbol) %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret",
                 type = 'log')
  #convert data to xts object
  returns_xts <- stocks_simple_portfolio %>% 
    select(symbol, date, ret) %>% 
    spread(symbol, value = ret) %>% 
    tk_xts()
  #calculate mean return
  mean_ret <- colMeans(returns_xts)
  cov_mat <- cov(returns_xts) * 252
  #create random weights
  #weights <- runif(n = 11, 0, 1)
  #weights <- weights/sum(weights)
  #generate matrix 
  #theoretical_ports <- 1000000
  all_weights <- matrix(nrow = 1000000,
                        ncol = 11)
  #empty vector to store portfolio returns
  port_returns <- vector('numeric', length=1000000)
  #same for risk
  port_stddev <- vector('numeric', length=1000000)
  port_sratios <- vector('numeric', length=1000000)
  #the loop 
  for(i in seq_along(port_returns)) {
    weights <- runif(n = 11, 0, 1)
    weights <- weights/sum(weights)
    #store theoretical weight in vector
    all_weights[i,] <- weights
    #
    port_ret <- sum(weights * mean_ret) 
    port_ret <- ((port_ret + 1)^252) - 1
    port_returns[i] <- port_ret
    #
    stddev <- sqrt(t(weights)) %*% (cov_mat %*% weights)
    port_stddev[i] <- stddev
    #
    #rfr = 0
    sratio <- port_ret / stddev
    port_sratios[i] <- sratio
  }
  theo_port_tib <- tibble(Return = port_returns,
                          Risk = port_stddev,
                          SharpeRatio = port_sratios)
  colnames(all_weights) <- colnames(returns_xts)
  
  theo_port_tib <- tk_tbl(cbind(all_weights, theo_port_tib))
  
  min_var <- theo_port_tib[which.min(theo_port_tib$Risk),]
  max_sharpe <- theo_port_tib[which.max(theo_port_tib$SharpeRatio),]
  high_return <- theo_port_tib[which.max(theo_port_tib$Return),]
#screenout suboptimal portfolios with too low returns  
  min_accept_return <- min_var %>% 
    select(Return)
#screen for max portfolios for the user return slider  
  num_ports_for_user_tib <-  theo_port_tib %>% 
    mutate(bin = cut_interval(Return, n = 50)) %>% 
    group_by(bin) %>% 
    arrange(desc(SharpeRatio)) %>% 
    mutate(rank = row_number(bin)) %>% 
    filter(rank == 1) %>% 
    filter(Return >= min_accept_return) %>% 
    count(bin =!1)
  
  num_ports_for_user <- num_ports_for_user_tib$n
  
  # Define server logic required to draw the UserweightsPlot
shinyServer(function(input, output) {
  #logic to define user portfolio reactive filter
  user_filter <- reactive({ 
    theo_port_tib %>% 
      mutate(bin = cut_interval(Return, n = 50)) %>% 
      group_by(bin) %>% 
      arrange(desc(SharpeRatio)) %>% 
      mutate(rank = row_number(bin)) %>% 
      filter(rank == 1) %>% 
      filter(Return >= min_accept_return) %>% 
      ungroup() %>% 
      arrange(Return) %>% 
      slice(input$Desired_Returns)
     })
  #used for portfolio growth and CAPM table
  
  output$UserweightsPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    q <- user_filter() %>% 
      gather(gld:vwo, key = Asset,
           value = Weights) %>%
      mutate(Asset = as.factor(Asset)) %>%
      ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
      geom_bar(stat = 'identity') +
      theme_minimal() +
      labs(x = 'Assets', y = 'Weights', title = "Your Optimal Portfolio") +
      scale_y_continuous(labels = scales::percent)
    ggplotly(q)
  })
 output$EfficientFrontier <-renderPlotly({
    ef <- theo_port_tib %>% 
      ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
      geom_point() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = 'Annualized Risk',
           y = 'Annualized Returns',
           title = "Efficient Frontier") +
      geom_point(aes(x = Risk,
                     y = Return), data = user_filter(), color = 'red') 
    ggplotly(ef)
    
  })
  output$PortfolioGrowth <- renderPlot({
    user_weights_vector <- user_filter() %>% 
        slice(1) %>% 
        c(., recursive = TRUE) %>% 
        unname()
      user_weights_vector <- user_weights_vector[1:11]
    portfolio_growth <- stocks_simple_portfolio %>% 
      tq_portfolio(assets_col = symbol,
                   returns_col = ret,
                   weights = user_weights_vector,
                   col_rename = "investment.growth",
                   wealth.index = TRUE) %>% 
      mutate(investment.growth = investment.growth * 10000)
    portfolio_growth %>% 
      ggplot(aes(x=date, y=investment.growth)) +
      geom_line(size =2, color=palette_light()[[1]]) +
      labs(title = "Portfolio Growth",
           subtitle = "Your Optimal Portfolio",
           caption = "Assuming Dividends Reinvested",
           x = "", y = "Portfolio Value") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
  })
})

