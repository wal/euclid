library(tidyverse)
library(lubridate)
library(shiny)
library(zoo)

DEFAULT_ACUTE_PERIOD = 7
DEFAULT_CHRONIC_PERIOD = 28

dataset <- read_csv('data/adam_sullivan_load_data.csv')
dataset$date <- dmy(dataset$date)
dataset$week <- week(dataset$date)
athletes <- unique(dataset$athlete)


unique(dataset$date)

# Application UI
ui <- fluidPage(
   
   # Application title
   titlePanel("TITLE"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("athlete", h4("Athlete"), choices = athletes, selected = head(athletes, 1)),
        p("Note: Acute Period is fixed at 7 Days"),
        sliderInput("chronic_period", h4("Chronic Period (days)"), min = 7, max = 28, step = 7, value = DEFAULT_CHRONIC_PERIOD)
      ),
      
      mainPanel(
        plotOutput("acrPlot"),
         plotOutput("acute_chronicPlot"),
        plotOutput("rawDataPlot")
      )
   )
)

server <- function(input, output) {

  acute_period <- reactive({
    DEFAULT_ACUTE_PERIOD
  })
  
  chronic_period <- reactive({
    input$chronic_period
  })
  
  filtered_dataset <- reactive({
    data <- dataset %>% filter(athlete == input$athlete)
    data %>% 
      mutate(acute = rollapply(load, acute_period(), FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
             chronic = rollapply(load, chronic_period(), FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
             acr = acute / chronic) 
  })

   output$rawDataPlot <- renderPlot({
     
      ggplot(filtered_dataset(), aes(date, load, fill = factor(week %% 2))) + 
        geom_bar(stat = "identity") +
        scale_color_discrete() +
        theme_minimal()
   })
   
   output$acute_chronicPlot <- renderPlot({
     ggplot(filtered_dataset(), aes(date)) + 
       geom_line(aes(y = acute), color = "blue") + 
       geom_line(aes(y = chronic), color = "red") + 
       theme_minimal()
   })
   
   output$acrPlot <- renderPlot({
     ggplot(filtered_dataset(), aes(date, acr)) + 
       geom_line() + 
       theme_minimal()
   })
}

shinyApp(ui = ui, server = server)

