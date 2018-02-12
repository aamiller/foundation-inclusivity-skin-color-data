#loading required libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(Hmisc)
library(shiny)
library(shinythemes)
library(dplyr)
library(scales)
library(knitr)

foundation_data <- read.csv("./data/cleaned_foundation_data.csv", stringsAsFactors = FALSE)
foundation_data$Brand.Tier <- factor(foundation_data$Brand.Tier, levels = c("Uber Luxury", "Highend/Luxury", "Sephora/Ulta", "Drugstore"))

# Server, handles user input and re-renders charts and tables
shinyServer(function(input, output) {
  
  #returns a graph that shows adjust rating affect the passing rate
  output$shadepricegraph <- renderPlotly({
    filtered.data <- foundation_data
    # Checkbox filters
    if (input$showDrugstore == FALSE) {
      filtered.data <- filter(filtered.data, Brand.Tier != "Drugstore")
    }
    if (input$showSephUlta == FALSE) {
      data.with.factors <- filter(filtered.data, Brand.Tier != "Sephora/Ulta")
    }
    if (input$showHighend == FALSE) {
      filtered.data <- filter(filtered.data, Brand.Tier != "Highend/Luxury")
    }
    if (input$showUberluxury == FALSE) {
      filtered.data <- filter(filtered.data, Brand.Tier != "Uber Luxury")
    }
    if (input$showDeepshades == FALSE) {
      filtered.data <- filter(filtered.data, Has.Darker.Shades != "1")
    }
    
    shade.v.price.graph <- plot_ly(data = filtered.data, x = ~Shade.Range, y = ~Price.Ounce, color = ~Brand.Tier,
                                   colors = "Set1", hoverinfo = "text", symbol = ~Has.Darker.Shades,symbols = c('x','o'),
                                   text = ~paste('<b>Price per Ounce (USD)</b>: $', Price.USD, "<br><b>Number of Shades</b>: ", Shade.Range, "<br><b>Brand</b>:", Brand, "<br><b>Product Name</b>: ", Foundation)) %>%
                                   layout(xaxis = list(title = "Number of shades for product"), yaxis = list(title = "Price per ounce", range = c(0, 250)), height = 600)
    return(shade.v.price.graph)
  })
  
  #returns a scatter plot of budget affect the passing rate
  output$shadepricefacetgraph <- renderPlot({
    filtered.facet.data <- foundation_data
    facet.graph <- ggplot(filtered.facet.data, aes(Shade.Range)) + geom_bar() + facet_wrap(~Brand.Tier) + xlab("Number of Shades For Product") + ylab("Number of Brands")
    return(facet.graph)  
  })
  
  #returns a map of countries affect the passing rate
  output$brandComparisonPlot <- renderPlotly({
    filtered.brand.data <- foundation_data %>% filter(Brand == input$brand1 | Brand == input$brand2 | Brand == input$brand3)
    filtered.brand.data$Brand <- factor(filtered.brand.data$Brand, levels = c(input$brand1, input$brand2, input$brand3))
    color.list = c("paleturquoise3", "dodgerblue3", "mediumorchid3")
    brandComparisonPlot <- plot_ly(data = filtered.brand.data, x = ~Shade.Range, y = ~Price.Ounce, color = ~Brand,
                                   colors = color.list, hoverinfo = "text", 
                                   text = ~paste('<b>Price per Ounce (USD)</b>: $', Price.USD, "<br><b>Number of Shades</b>: ", Shade.Range, "<br><b>Brand</b>:", Brand, "<br><b>Product Name</b>: ", Foundation)) %>%
      layout(xaxis = list(title = "Number of shades for product"), yaxis = list(title = "Price per ounce"), title = "Price per Ounce vs. Number of Shades Brand Comparison")
    
    return(brandComparisonPlot)
  })
  
  output$foundationData <- renderDataTable({
    return(foundation_data)
  })
  
  ## WIP
  output$aggregatebrandcomparison <- renderPlotly({
    filtered.data <- foundation_data %>% group_by(Brand) %>% summarize("Average Shades in Range" = mean(Shade.Range))
    aggregate.brand.comparison.graph <- plot_ly(data = filtered.data, x = ~ Brand, y = ~ Shade.Range, hoverinfo = "text", 
                                                text = ~paste('<b>Price per Ounce (USD)</b>: $', Price.USD, "<br><b>Number of Shades</b>: ", Shade.Range, "<br><b>Brand</b>:", Brand, "<br><b>Product Name</b>: ", Foundation))
    return(aggregate.brand.comparison.graph)
  })
  
})
