library(tidyverse)
library(lubridate)
library(shiny)
library(tidyquant)
library(purrr)
library(zoo)
library(qcc)

DEFAULT_ACUTE_PERIOD = 7
DEFAULT_CHRONIC_PERIOD = 28
ACR_METHODS = c("Simple Rolling Average" = "simple_ra", 
                "EWMA" = "ewma")

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
        sliderInput("ac_periods", 
                    h4("Acute - Chronic Periods (days)"), 
                    min = 7, 
                    max = 42, 
                    step = 7, 
                    value = c(DEFAULT_ACUTE_PERIOD, DEFAULT_CHRONIC_PERIOD)),
        checkboxGroupInput("methods", h4("Analysis Method"), ACR_METHODS, selected=ACR_METHODS[1])
        ),
      mainPanel(
        plotOutput("acrPlot"),
        dataTableOutput("acrData")
      )
   )
)

server <- function(input, output) {

  acute_period <- reactive({
    input$ac_periods[1]
  })
  
  chronic_period <- reactive({
    input$ac_periods[2]
  })
  
  filtered_data <- reactive({
    dataset %>% filter(athlete == input$athlete)
  })
  
  simple_rolling_average_AC <- function(data, acute_period, chronic_period) {
    data %>% 
      mutate(
        method = "simple_ra",
        acute = rollapply(load, acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        chronic = rollapply(load, chronic_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  ewma_AC <- function(data, acute_period, chronic_period) {
    #mutate(Chronic_weighted = Player.Load *0.07 + (1-0.07) * lag(Chronic),
    #       Acute_weighted = Player.Load * 0.25 + (1-0.25)* lag(Acute))
    
    acute_alpha <- 0.25 #2/(acute_period+1)
    chronic_alpha <- 0.09 #2/(chronic_period+1)
    
    data %>%
      mutate(
        method = "ewma",
        acute = load * acute_alpha + (1-acute_alpha) * lag(load),
        chronic = load * chronic_alpha + (1-chronic_alpha) * lag(load),
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  perform_analysis <- function(x) {
    analysis_type <- x
    data <- filtered_data()
    acute_period <- acute_period()
    chronic_period <- chronic_period()
    
    if(analysis_type == "simple_ra") {
      simple_rolling_average_AC(data, acute_period, chronic_period)
    } else {
      ewma_AC(data, acute_period, chronic_period)
    } 
  }
  
  analysed_data <- reactive({
    print(input$methods)
    input$methods %>% map(perform_analysis) %>% reduce(rbind)
  })

   output$acrPlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'acr')
     ggplot(data, aes(date, value, color = method)) + 
       geom_line() + 
       theme_minimal()
   })
   
   output$acrData <- renderDataTable({
     analysed_data() %>% spread(statistic, value)
   })
}

shinyApp(ui = ui, server = server)

