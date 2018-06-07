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
                "EWMA Coupled" = "ewma_coupled",
                "EWMA Un-Coupled" = "ewma_uncoupled")

dataset <- read_csv('data/adam_sullivan_load_data.csv')
dataset$date <- dmy(dataset$date)
dataset$week <- week(dataset$date)
athletes <- unique(dataset$athlete)


# Application UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Acute/Chronic Workload Ratio"),
   
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
        acute = rollapply(load, acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        chronic = rollapply(load, chronic_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  ewma_coupled_AC <- function(data, acute_period, chronic_period) {
    acute_alpha <- 2/(acute_period+1)
    chronic_alpha <- 2/(chronic_period+1)
    
    data$acute <- NA
    data$chronic <- NA
    
    data$acute[1] <- data$load[1]
    data$chronic[1] <- data$load[1]

    for(i in (2:dim(data)[1])){
      data$acute[i] <- data$load[i] * acute_alpha + (1-acute_alpha) * data$acute[i-1]
      data$chronic[i] <- data$load[i] * chronic_alpha + (1-chronic_alpha) * data$chronic[i-1]
    }
    
    data %>%
      mutate(
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  ewma_uncoupled_AC <- function(data, acute_period, chronic_period) {
    acute_alpha <- 2/(acute_period+1)
    chronic_alpha <- 2/(chronic_period+1)
    
    data$acute <- NA
    data$chronic <- NA
    
    data$acute[1] <- data$load[1]
    data$chronic[1:8] <- mean(data$load[1:7])
    
    for(i in (2:dim(data)[1])){
      data$acute[i] <- data$load[i] * acute_alpha + (1-acute_alpha) * data$acute[i-1]
      if(i > 8) {
        data$chronic[i] <- data$load[i-7] * chronic_alpha + (1-chronic_alpha) * data$chronic[i-8]
      }
    }
    
    data %>%
      mutate(
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  perform_analysis <- function(x) {
    analysis_type <- x
    data <- filtered_data()
    acute_period <- acute_period()
    chronic_period <- chronic_period()
    
    if(analysis_type == "simple_ra") {
      simple_rolling_average_AC(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else if (analysis_type == "ewma_coupled") {
      ewma_coupled_AC(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else {
      ewma_uncoupled_AC(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
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
     analysed_data() %>% 
       spread(statistic, value)
   })
}

shinyApp(ui = ui, server = server)

