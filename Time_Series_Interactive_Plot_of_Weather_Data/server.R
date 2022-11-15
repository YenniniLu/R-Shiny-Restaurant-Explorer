# Create a Shiny app to display an interactive time series plot of the macleish weather data. 
# Include a selection box to alternate between data from the whately_2015 and orchard_2015 weather stations. 
# Add a selector of dates to include in the display. Do you notice any irregularities?

#loading libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(macleish)

dates1 <- whately_2015 %>% mutate(when = date(when)) %>% group_by(when) %>% select(when) %>% unique()
dates2 <- orchard_2015 %>% mutate(when = date(when)) %>% group_by(when) %>% select(when) %>% unique()


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "whately_2015" = whately_2015,
           "orchard_2015" = orchard_2015)
  })
  
  datasetDates <- reactive({  # dynamic list of gender
    req(input$dates)   # wait until list is available
    data <- datasetInput() 
    if (input$dates != "ALL") {
      data <- data %>%
        filter(date(when) == input$dates)
    }
    data
  })
  
  output$dates <- renderUI({
    if (input$dataset == "whately_2015") {
      selectInput("dates", "Pick one date:", c("ALL",dates1))
    }
    else if (input$dataset == "orchard_2015") {
      selectInput("dates", "Pick one date:", c("ALL",dates2))
    }
    
  })
  
  output$weatherVars <- renderUI({
    
    if (input$dataset == "whately_2015") {
      selectInput("vars", "Pick one variable:", names(whately_2015)[-1])
    }
    else if (input$dataset == "orchard_2015") {
      selectInput("vars", "Pick one variable:", names(orchard_2015)[-1])
    }
    
  })
  
  output$info <- renderText({
    dates <- as_datetime(input$plot_click$x)
    paste0("x=", dates, "\ny=", input$plot_click$y)
  })
  
  output$weatherPlot <- renderPlot({
    
    data1 <- datasetDates()
    plot(as.formula(paste(input$vars, " ~ when")), data = data1)
  })
  
  
  
}
