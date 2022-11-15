# Create a Shiny app to display an interactive time series plot of the macleish weather data. 
# Include a selection box to alternate between data from the whately_2015 and orchard_2015 weather stations. 
# Add a selector of dates to include in the display. Do you notice any irregularities?

#loading libraries
library(shiny)
library(tidyverse)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Interactive time series plot of the macleish weather data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("whately_2015", "orchard_2015")),
      uiOutput("weatherVars"),
      uiOutput("dates")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #tableOutput("table"),
      plotOutput("weatherPlot", click = "plot_click"),
      verbatimTextOutput("info")
    )
  )
)