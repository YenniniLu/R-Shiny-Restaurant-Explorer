# Problem 7 (Medium): 
# Using data from the palmerpenguins package, create a Shiny app that displays measurements from the penguins dataframe. 
# Allow the user to select a species or a gender, and to choose between various attributes on a scatterplot. 

library(tidyverse)
library(palmerpenguins)
library(shiny)
library(shinythemes)


vars <- setdiff(names(penguins), c("species", "island", "sex", "year"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                pageWithSidebar(headerPanel = "Penguins Explorer",
                                sidebarPanel(
                                  selectInput(
                                    inputId = "species",
                                    "Species:",
                                    choices = c(
                                      "ALL",
                                      unique(as.character(penguins$species))
                                    )
                                  ),
                                  uiOutput("gendercontrols"),
                                  selectInput('xcol', 'X Variable', vars),
                                  selectInput('ycol', 'Y Variable', vars)
                                  ),
                                mainPanel(
                                  plotOutput('plot1')
                                )
                                )
                )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetspecies <- reactive({  # Filter data based on selections
    data <- penguins %>%
      select(
        species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex
      ) %>%
      distinct()
    req(input$species)  # wait until there's a selection
    if (input$species != "ALL") {
      data <- data %>%
        filter(species == input$species)
    }
    data
  })
  
  datasetgender <- reactive({  # dynamic list of gender
    req(input$sex)   # wait until list is available
    data <- datasetspecies() %>%
      unique()
    if (input$sex != "ALL") {
      data <- data %>%
        filter(sex == input$sex)
    }
    data
  })
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data <- datasetgender() 
    data[, c(input$xcol, input$ycol)]
  })
  
  output$plot1 <- renderPlot({
    plot(selectedData(), pch = 16, cex = 1, col = "#333333")
  }, res = 96)
  
  
  
  output$gendercontrols <- renderUI({
    availablelevels <-
      unique(sort(as.character(datasetspecies()$sex)))
    selectInput(
      inputId = "sex",
      label = "Gender:",
      choices = c("ALL", availablelevels)
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
