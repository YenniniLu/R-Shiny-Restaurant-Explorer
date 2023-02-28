# Load packages -----------------------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(gtrendsR)

# Load data ---------------------------------------------------------
res <- gtrends(c("Taylor Swift","Justin Bieber", "BLACKPINK", "BTS", "Ariana Grande"), 
               geo = c("US"), time = "2008-01-01 2023-02-27")
#plot(res)
res <- res$interest_over_time
#head(res)
res %>% select(keyword, date, hits) %>%
  rename(type = keyword, close = hits) %>%
  write_csv("trend_data_singers.csv")

trend_data_singers <- read_csv("trend_data_singers.csv")

trend_description <- data.frame(type = c("Taylor Swift","Justin Bieber", "BLACKPINK", "BTS", "Ariana Grande"),
                                text = c("Taylor Swift released her first song on June 19, 2006, and we see June 19, 2006 the beginning of her music career.",
                                         "Justin Bieber released his first song on May 18, 2009, and we see May 18, 2009 the beginning of his music career. The 0 search before May 18, 2009 on Google is reasonable.",
                                         "BLACKPINK released their first song on August 8, 2016, and we see August 8, 2016 the beginning of their music career. The 0 search before August 8, 2016 on Google is reasonable.",
                                         "BTS released their first song on June 12, 2013, and we see June 12, 2013 the beginning of their music career. The 0 search before June 12, 2013 on Google is reasonable.",
                                         "Ariana Grande released her first song on December 12, 2011, and we see December 12, 2011 the beginning of her music career. The 0 search before December 12, 2011 on Google is reasonable."))

# Define UI ---------------------------------------------------------
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Google Trend Index of Singers"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Trend index"),
                                choices = unique(trend_data_singers$type),
                                selected = "Taylor Swift"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), 
                                   start = "2008-01-01", end = "2023-02-27",
                                   min = "2008-01-01", max = "2023-02-27"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", 
                                  label = strong("Overlay smooth trend line"), 
                                  value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function --------------------------------------------
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data_singers %>%
      filter(
        type == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2008 and is calculated only for US search traffic.")
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)