# Problem 7 (Medium): 
# Using data from the palmerpenguins package, create a Shiny app that displays measurements from the penguins dataframe. 
# Allow the user to select a species or a gender, and to choose between various attributes on a scatterplot. 

library(tidyverse)
library(palmerpenguins)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                pageWithSidebar(headerPanel = "Penguins Explorer",
                                sidebarPanel(
                                  selectInput(
                                    penguins$species,
                                    "Species:",
                                    c(
                                      "Adelie" = "Adelie",
                                      "Gentoo" = "Gentoo",
                                      "Chinstrap" = "Chinstrap"
                                    )
                                  ),
                                  selectInput(penguins$sex,
                                              "Gender:",
                                              c("Male" = "male",
                                                "Female" = "female"))
                                  ),
                                mainPanel(
                                  plotOutput('plot1')
                                )
                                )
                )


# Define server logic required to draw a histogram
server <- function(input, output) {
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
