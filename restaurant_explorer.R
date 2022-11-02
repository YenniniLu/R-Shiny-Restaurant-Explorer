## Problem 4 (Medium): 
# Modify the restaurant violations Shiny app so that it displays a table of the number of restaurants within a given type of cuisine along with a count of restaurants as specified by the dba variable. 
#(Hint: Be sure not to double count. The dataset should include 842 unique pizza restaurants in all boroughs and 281 Caribbean restaurants in Brooklyn.)

#loading libraries
library(shiny)
library(shinythemes)
library(mdsr)
library(tidyverse)

mergedViolations <- Violations %>%
  left_join(Cuisines)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Restaurant Explorer",
                  tabPanel("Overview",              # Overview panel
                           DT::dataTableOutput('overview')), 
                  
                  tabPanel(                         # Cuisine panel
                    "Borough and Cuisines",
                    fluidRow(
                      # some things take time: this lets users know
                      add_busy_spinner(spin = "fading-circle"),
                      column(4,
                             selectInput(
                               inputId = "boro",
                               label = "Borough:",
                               choices = c("ALL",
                                           unique(as.character(
                                             mergedViolations$boro
                                           )))
                             )),
                      # display dynamic list of cuisines
                      column(4, uiOutput("cuisinecontrols"))
                    ),
                    # Create a new row for the table.
                    fluidRow(DT::dataTableOutput("table"))
                  )
                ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # load and wrangling the data for display: Overview Panel
  data1 <- mergedViolations %>% 
    select(boro, dba, cuisine_description) %>% 
    distinct() %>% 
    group_by(cuisine_description) %>% 
    count() %>% 
    ungroup()
  
  # load and wrangling the data for display: Borough and Cuisines Panel
  datasetboro <- reactive({  # Filter data based on selections
    data <- mergedViolations %>% 
      select(boro, dba, cuisine_description) %>% 
      distinct() %>% 
      group_by(cuisine_description, boro) %>% 
      count() %>% 
      ungroup()
    req(input$boro)  # wait until there's a selection
    if (input$boro != "ALL") {
      data <- data %>%
        filter(boro == input$boro)
    }
    data
  })
  
  datasetcuisine <- reactive({  # dynamic list of cuisines
    req(input$cuisine)   # wait until list is available
    data <- datasetboro() %>%
      unique()
    if (input$cuisine != "ALL") {
      data <- data %>%
        filter(cuisine_description == input$cuisine)
    }
    data
  })
  
  output$cuisinecontrols <- renderUI({ 
    availablelevels <-
      unique(sort(as.character(datasetboro()$cuisine_description)))
    selectInput(
      inputId = "cuisine",
      label = "Cuisine:",
      choices = c("ALL", availablelevels)
    )
  })
  
  output$overview <- DT::renderDataTable(DT::datatable(data1))         # output: Overview Panel
  output$table <- DT::renderDataTable(DT::datatable(datasetcuisine())) # Output: Borough and Cuisines Panel
}

# Run the application
shinyApp(ui = ui, server = server)
