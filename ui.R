library(dplyr)
library(ggplot2)
library(plotly)
library(Hmisc)
library(shiny)
library(shinythemes)
library(dplyr)
library(scales)
library(knitr)

# Sourcing functions in different file 


# Read in foundation data
foundation_data <- read.csv("./data/cleaned_foundation_data.csv", stringsAsFactors = FALSE)

# Shiny App UI 
shinyUI(shinyUI(navbarPage(theme = shinythemes::shinytheme("sandstone"),
                           title = "Foundation Shade Report",
                           
                           # Home page - landing, introduces project
                           tabPanel(h5("Home")),
                           
                           # Shade inclusivity tab
                           tabPanel(h5("Shade vs. Price (1)"), 
                                    tags$h3(class = "header",'Price of Foundation vs. Shades Available'),
                                      mainPanel(
                                      plotlyOutput('shadepricegraph')
                                     ),
                                    
                                    #sidebar panel for explanation about different rating 
                                    sidebarPanel(
                                      tags$h2(class = "header", "Content Ratings Guide", align = "center"),
                                      tags$p(class = "guide", strong("Drugstore -- "), "Maybelline, L'oreal, Revlon etc.", align = "center"),
                                      tags$p(class = "guide", strong("Sephora/Ulta --"), "NARS, Bobbi Brown, MAC etc.", align = "center"),
                                      tags$p(class = "guide", strong("High-end/Luxury -- "), "YSL, Lancôme etc.", align = "center"),
                                      tags$p(class = "guide", strong("Uber Luxury -- "), "La Mer, Siley etc.", align = "center"),
                                      tags$p(class = "guide", "Brand descriptions collected from gills__'s original Reddit post and separated based on her designations. They are not ordered by price.", align = "center"),
                                      checkboxInput('showDrugstore', "Show drugstore brands", value = TRUE),
                                      checkboxInput('showSephUlta', "Show Sephora/Ulta brands", value = TRUE),
                                      checkboxInput('showHighend', "Show High-end/Luxury brands", value = TRUE),
                                      checkboxInput('showUberluxury', "Show Uber Luxury brands", value = TRUE),
                                      checkboxInput('showDeepshades', "Show foundations with deep shades", value = TRUE)
                                      )
                                      ),
                           tabPanel(h5("Shade vs. Price (2)"), 
                                    tags$h3(class = "header", 'Shades Number by Brand Tier'),
                                    mainPanel(
                                      plotOutput('shadepricefacetgraph')
                                    ),
                                    
                                    #sidebar panel for explanation about different rating 
                                    sidebarPanel(
                                      tags$h2(class = "header", "Content Ratings Guide", align = "center"),
                                      tags$p(class = "guide", strong("Drugstore -- "), "Maybelline, L'oreal, Revlon etc.", align = "center"),
                                      tags$p(class = "guide", strong("Sephora/Ulta --"), "NARS, Bobbi Brown, MAC etc.", align = "center"),
                                      tags$p(class = "guide", strong("High-end/Luxury -- "), "YSL, Lancôme etc.", align = "center"),
                                      tags$p(class = "guide", strong("Uber Luxury -- "), "La Mer, Siley etc.", align = "center"),
                                      tags$p(class = "guide", "Brand descriptions collected from gills__'s original Reddit post and separated based on her designations. They are not ordered by price.", align = "center")                                    )
                           ),
                           
                           # Profit/Budget tab 
                           tabPanel(h5("Brand Comparison (1)"), 
                                    tags$h3(class = "header", 'How do different brands compare?'),
                                    sidebarLayout(
                                      
                                      # Side panel for controls
                                      sidebarPanel(
                                        # Input to select the brands to Map
                                        selectInput('brand1', label = 'Brand 1 to Display', choices = list("MAC" = 'MAC', "Makeup Forever" = 'MUFE', "Fenty Beauty" = 'Fenty Beauty', "Lacome" = 'Lacome', "NARS" = 'NARS',
                                                                                                           "Hourglass" = 'Hourglass', "Maybelline" = 'Maybelline', "L'Oreal" = "L'Oreal", "Bobbi Brown" = 'Bobbi Brown',
                                                                                                           "Kat Von D" = 'KVD', "Bare Minerals" = 'BareMinerals', "Anastasia Beverly Hills" = 'ABH', "Hudy Beauty" = 'Huda',
                                                                                                           "NYX" = 'NYX', "Urban Decay" = "Urban Decay", "Cover FX" = "Cover FX", "Dior" = "Dior", "Giorgio Armani" = "Giorgio Armani",
                                                                                                           "Too Faced" = "Too Faced", "Tarte" = 'Tarte', "Smashbox" = 'SmashBox', "Yves Saint Laurent" = 'YSL', "Marc Jacobs" = 'Marc Jacobs',
                                                                                                           "Revlon" = 'Revlon', "Clinique" = "Clinque", "The Ordinary Foundations" = 'The Ordinary Foundations', "Wet 'n' Wild" = "Wet 'n' Wild",
                                                                                                           "La Mer" = 'La Mer', "Laura Mercier" = 'Laura Mercier', "Milani" = 'Milani', "Kokie" = "Kokie", "Jouer" = 'Jouer', "Stila" = 'Stila',
                                                                                                           "Physician's Formula" = "Physician's Formula", "Milk Makeup" = "MILK", "L.A. Girl" = 'L.A. Girl', "Rimmel" = 'Rimmel', "Elizabeth Arden" = "Elizabeth Arden",
                                                                                                           "Neutrogena" = 'Neutrogena', 'Covergirl' = "Covergirl", "Terry Terrybly" = 'Terry Terrybly', "CHANTECAILLE" = 'CHANTECAILLE', "Burberry" = "Burberry",
                                                                                                           "Josie Maran" = 'Josie Maran', "la Prairie" = "La Prairie", "Kevyn Aucoin" = "Kevyn Aucoin", "Natasha Denona" = 'Natasha Denona',
                                                                                                           "CHANEL" = "CHANEL", "Givenchy" = "Givenchy", "Cle De Peu" = "Cle De Peu", "Charlotte Tilbury" = "Charlotte Tilbury",
                                                                                                           "Laura Geller" = "Laura Geller", "Benefit" = "Benefit", "BECCA" = "BECCA", "Tom Ford" = "Tom Ford", "It Cosmetics" = "It Cosmetics",
                                                                                                           "Koh Gen Do" = "Koh Gen Do", "Pur" = "Pur", "Soap & Glory" = "Soap & Glory", "Burt's Bees" = "Burt's Bees", "Perricone" = "Perricone"), selected = "MAC"),
                                        selectInput('brand2', label = 'Brand 2 to Display', choices = list("MAC" = 'MAC', "Makeup Forever" = 'MUFE', "Fenty Beauty" = 'Fenty Beauty', "Lacome" = 'Lacome', "NARS" = 'NARS',
                                                                                                                  "Hourglass" = 'Hourglass', "Maybelline" = 'Maybelline', "L'Oreal" = "L'Oreal", "Bobbi Brown" = 'Bobbi Brown',
                                                                                                                  "Kat Von D" = 'KVD', "Bare Minerals" = 'BareMinerals', "Anastasia Beverly Hills" = 'ABH', "Hudy Beauty" = 'Huda',
                                                                                                                  "NYX" = 'NYX', "Urban Decay" = "Urban Decay", "Cover FX" = "Cover FX", "Dior" = "Dior", "Giorgio Armani" = "Giorgio Armani",
                                                                                                                  "Too Faced" = "Too Faced", "Tarte" = 'Tarte', "Smashbox" = 'SmashBox', "Yves Saint Laurent" = 'YSL', "Marc Jacobs" = 'Marc Jacobs',
                                                                                                                  "Revlon" = 'Revlon', "Clinique" = "Clinque", "The Ordinary Foundations" = 'The Ordinary Foundations', "Wet 'n' Wild" = "Wet 'n' Wild",
                                                                                                                  "La Mer" = 'La Mer', "Laura Mercier" = 'Laura Mercier', "Milani" = 'Milani', "Kokie" = "Kokie", "Jouer" = 'Jouer', "Stila" = 'Stila',
                                                                                                                  "Physician's Formula" = "Physician's Formula", "Milk Makeup" = "MILK", "L.A. Girl" = 'L.A. Girl', "Rimmel" = 'Rimmel', "Elizabeth Arden" = "Elizabeth Arden",
                                                                                                                  "Neutrogena" = 'Neutrogena', 'Covergirl' = "Covergirl", "Terry Terrybly" = 'Terry Terrybly', "CHANTECAILLE" = 'CHANTECAILLE', "Burberry" = "Burberry",
                                                                                                                  "Josie Maran" = 'Josie Maran', "la Prairie" = "La Prairie", "Kevyn Aucoin" = "Kevyn Aucoin", "Natasha Denona" = 'Natasha Denona',
                                                                                                                  "CHANEL" = "CHANEL", "Givenchy" = "Givenchy", "Cle De Peu" = "Cle De Peu", "Charlotte Tilbury" = "Charlotte Tilbury",
                                                                                                                  "Laura Geller" = "Laura Geller", "Benefit" = "Benefit", "BECCA" = "BECCA", "Tom Ford" = "Tom Ford", "It Cosmetics" = "It Cosmetics",
                                                                                                                  "Koh Gen Do" = "Koh Gen Do", "Pur" = "Pur", "Soap & Glory" = "Soap & Glory", "Burt's Bees" = "Burt's Bees", "Perricone" = "Perricone"), selected = "Revlon"),
                                        selectInput('brand3', label = 'Brand 3 to Display', choices = list("MAC" = 'MAC', "Makeup Forever" = 'MUFE', "Fenty Beauty" = 'Fenty Beauty', "Lacome" = 'Lacome', "NARS" = 'NARS',
                                                                                                                  "Hourglass" = 'Hourglass', "Maybelline" = 'Maybelline', "L'Oreal" = "L'Oreal", "Bobbi Brown" = 'Bobbi Brown',
                                                                                                                  "Kat Von D" = 'KVD', "Bare Minerals" = 'BareMinerals', "Anastasia Beverly Hills" = 'ABH', "Hudy Beauty" = 'Huda',
                                                                                                                  "NYX" = 'NYX', "Urban Decay" = "Urban Decay", "Cover FX" = "Cover FX", "Dior" = "Dior", "Giorgio Armani" = "Giorgio Armani",
                                                                                                                  "Too Faced" = "Too Faced", "Tarte" = 'Tarte', "Smashbox" = 'SmashBox', "Yves Saint Laurent" = 'YSL', "Marc Jacobs" = 'Marc Jacobs',
                                                                                                                  "Revlon" = 'Revlon', "Clinique" = "Clinque", "The Ordinary Foundations" = 'The Ordinary Foundations', "Wet 'n' Wild" = "Wet 'n' Wild",
                                                                                                                  "La Mer" = 'La Mer', "Laura Mercier" = 'Laura Mercier', "Milani" = 'Milani', "Kokie" = "Kokie", "Jouer" = 'Jouer', "Stila" = 'Stila',
                                                                                                                  "Physician's Formula" = "Physician's Formula", "Milk Makeup" = "MILK", "L.A. Girl" = 'L.A. Girl', "Rimmel" = 'Rimmel', "Elizabeth Arden" = "Elizabeth Arden",
                                                                                                                  "Neutrogena" = 'Neutrogena', 'Covergirl' = "Covergirl", "Terry Terrybly" = 'Terry Terrybly', "CHANTECAILLE" = 'CHANTECAILLE', "Burberry" = "Burberry",
                                                                                                                  "Josie Maran" = 'Josie Maran', "la Prairie" = "La Prairie", "Kevyn Aucoin" = "Kevyn Aucoin", "Natasha Denona" = 'Natasha Denona',
                                                                                                                  "CHANEL" = "CHANEL", "Givenchy" = "Givenchy", "Cle De Peu" = "Cle De Peu", "Charlotte Tilbury" = "Charlotte Tilbury",
                                                                                                                  "Laura Geller" = "Laura Geller", "Benefit" = "Benefit", "BECCA" = "BECCA", "Tom Ford" = "Tom Ford", "It Cosmetics" = "It Cosmetics",
                                                                                                                  "Koh Gen Do" = "Koh Gen Do", "Pur" = "Pur", "Soap & Glory" = "Soap & Glory", "Burt's Bees" = "Burt's Bees", "Perricone" = "Perricone"), selected = "Benefit")
                                        
                                      ),
                                      
                                      # Main panel: display plotly map
                                      mainPanel(
                                        plotlyOutput('brandComparisonPlot')
                                        )
                                        )
                                        ),
                           
                           tabPanel(h5("Shade Range by Brand"), 
                                    tags$h3(class = "header",'Shade Range by Brand'),
                                    mainPanel(
                                      plotlyOutput('aggregatebrandcomparison')
                                    ),
                                    
                                    #sidebar panel for explanation about different rating 
                                    sidebarPanel(
                                      tags$h2(class = "header", "Content Ratings Guide", align = "center"),
                                      tags$p(class = "guide", strong("Drugstore -- "), "Maybelline, L'oreal, Revlon etc.", align = "center"),
                                      tags$p(class = "guide", strong("Sephora/Ulta --"), "NARS, Bobbi Brown, MAC etc.", align = "center"),
                                      tags$p(class = "guide", strong("High-end/Luxury -- "), "YSL, Lancôme etc.", align = "center"),
                                      tags$p(class = "guide", strong("Uber Luxury -- "), "La Mer, Siley etc.", align = "center"),
                                      tags$p(class = "guide", "Brand descriptions collected from gills__'s original Reddit post and separated based on her designations. They are not ordered by price.", align = "center")
                                    )
                           ),
                           
                           # Search foundations tab
                           tabPanel(h5("Search foundations"),
                                    tags$h3("Foundation data", class = "header", align = "center"),
                                    dataTableOutput("foundationData")
                           ),
                           
                           # About project tab
                           tabPanel(h5("About the Project"),
                                    tags$h1("About", align = "center", class = "header"),
                                    tags$p("", align = "center"),
                                    tags$p("among other things.", align = "center")
                                    )
                                      )                           
                                      ))