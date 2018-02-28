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
library(stringr)

foundation_data <- read.csv("./data/cleaned_foundation_data.csv", stringsAsFactors = FALSE)
foundation_data$Brand.Tier <- factor(foundation_data$Brand.Tier, levels = c("Uber Luxury", "Highend/Luxury", "Sephora/Ulta", "Drugstore"))


# Server, handles user input and re-renders charts and tables
shinyServer(function(input, output) {
  
  #returns a graph that shows adjust rating affect the passing rate
  output$shadepricegraph <- renderPlotly({
    filtered.data <- foundation_data
    
    filtered.data$Has.Darker.Shades[foundation_data$Has.Darker.Shades == 1] <- "Has darker shades"
    filtered.data$Has.Darker.Shades[foundation_data$Has.Darker.Shades == 0] <- "No darker shades"
    for(row in length(filtered.data$Has.Darker.Shades)) {
      if (filtered.data$Has.Darker.Shades[row] == "Has darker shades") {
      filtered.data$Brand.Tier[row] <- paste0(filtered.data$Brand.Tier[row], " Has Dark Shades")
      } else if(filtered.data$Has.Darker.Shades[row] == "No darker shades") {
        if(is.na(filtered.data$Brand.Tier[row])) {
          filtered.data$Brand.Tier[row] <- paste0(filtered.data$Brand.Tier[row], " No Dark Shades")
        }
      }
    }
    
    shade.v.price.graph <- plot_ly(data = filtered.data, x = ~Shade.Range, y = ~Price.Ounce, color = ~Brand.Tier,
                                   colors = "Set1", hoverinfo = "text", symbol = ~Has.Darker.Shades,symbols = c('x','check'),
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
  
  
  output$aggregatebrandcomparison <- renderPlotly({
    filtered.data <- foundation_data %>% group_by(Brand) %>% summarise("Average.Shades.per.Foundation" = mean(Shade.Range)) %>% arrange(desc(Average.Shades.per.Foundation))
    
    # Add column for whether or not brand has any foundations with darker shades
    # LA MER is filtered out by removing NAs
    foundation_data$Has.Darker.Shades <- as.numeric(foundation_data$Has.Darker.Shades) # Fix type of 0s and ones so can be summed
    filtered.data_has_darker_shades_sum <- foundation_data %>% group_by(Brand) %>% summarise("Foundations.with.darker.shades" = sum(Has.Darker.Shades, na.rm = TRUE)) %>% arrange(desc(Foundations.with.darker.shades))
    
    inclusive.foundation.percent.by.brand <- as.data.frame(brands)
    inclusive.foundation.percent.by.brand$counts <- 0
    for (brand in foundation_data$Brand) {
      inclusive.foundation.percent.by.brand[inclusive.foundation.percent.by.brand$brand == brand, "counts"] <- inclusive.foundation.percent.by.brand[inclusive.foundation.percent.by.brand$brand == brand, "counts"] + 1
    }
    shade.count <- foundation_data %>% group_by(Brand) %>% summarise("Sum.Shades" = sum(Shade.Range))
    
    
    
    # Add column to main data
    filtered.data$Foundations.with.darker.shades <- filtered.data_has_darker_shades_sum$Foundations.with.darker.shades
    
    aggregate.brand.comparison.graph <- ggplot(filtered.data, aes(x = Brand, y = Average.Shades.per.Foundation)) + geom_bar(stat="identity") + facet_wrap(~ Foundations.with.darker.shades)
    return(aggregate.brand.comparison.graph)
  })
  
  
})
