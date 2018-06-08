library(tidyverse)
library(lubridate)
library(shiny)
library(tidyquant)
library(purrr)
library(zoo)
library(qcc)
library(ggExtra)
library(ggpubr)
library(shinythemes)

DEFAULT_ACUTE_PERIOD = 7
DEFAULT_CHRONIC_PERIOD = 28
ACR_METHODS = c("Simple Rolling Average Coupled" = "simple_ra_coupled", 
                "Simple Rolling Average Uncoupled" = "simple_ra_uncoupled", 
                "EWMA Coupled" = "ewma_coupled",
                "EWMA Uncoupled" = "ewma_uncoupled")

dataset <- read_csv('data/adam_sullivan_load_data.csv')
dataset$date <- dmy(dataset$date)
dataset$week <- week(dataset$date)
athletes <- unique(dataset$athlete)


# Application UI
ui <- fluidPage(theme = shinytheme("flatly"),
   
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
        checkboxGroupInput("methods", h4("Analysis Method"), ACR_METHODS, selected=ACR_METHODS)
        ),
      mainPanel(
        tabsetPanel(
          tabPanel("Acute / Chronic Ratio",
            plotOutput("acrPlot"),
            hr(),hr(),
            plotOutput("acutePlot"),
            hr(),hr(),
            plotOutput("chronicPlot")
          ),
          tabPanel("Correlation",
            plotOutput("simple_ra_coupled_correlationPlot"),
            hr(),hr(),
            plotOutput("simple_ra_uncoupled_correlationPlot"),
            hr(),hr(),
            plotOutput("ewma_coupled_correlationPlot"),
            hr(),hr(),
            plotOutput("ewma_uncoupled_correlationPlot")
          )
        )
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
  
  simple_rolling_average_coupled <- function(data, acute_period, chronic_period) {
    data %>% 
      mutate(
        acute = rollapply(load, acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        chronic = rollapply(load, chronic_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  simple_rolling_average_uncoupled <- function(data, acute_period, chronic_period) {
    data %>% 
      mutate(
        acute = rollapply(load, acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        chronic = rollapply(load, c(list(-acute_period:-(chronic_period+acute_period)), -chronic_period), FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
        acr = acute / chronic) %>%
      gather(statistic, value, acute, chronic, acr)
  }
  
  ewma_coupled <- function(data, acute_period, chronic_period) {
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
  
  ewma_uncoupled <- function(data, acute_period, chronic_period) {
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
    
    if(analysis_type == "simple_ra_coupled") {
      simple_rolling_average_coupled(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else if(analysis_type == "simple_ra_uncoupled") {
      simple_rolling_average_uncoupled(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else if (analysis_type == "ewma_coupled") {
      ewma_coupled(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else {
      ewma_uncoupled(data, acute_period, chronic_period) %>% mutate(method = analysis_type)
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
       xlab(NULL) + ylab(NULL)+
       theme_minimal() +
       ggtitle("Acute/Chronic Ratio", subtitle = "bish bash bosh")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
   })
   
   output$acutePlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'acute')
     ggplot(data, aes(date, value, color = method)) + 
       geom_line() + 
       theme_minimal() +
       ggtitle("Acute Workload")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
   })
   
   output$chronicPlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'chronic')
     ggplot(data, aes(date, value, color = method)) + 
       geom_line() + 
       theme_minimal() +
       ggtitle("Chronic Workload")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
   })
   
   output$simple_ra_coupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'simple_ra_coupled') %>%
       spread(statistic, value)
    
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       ggtitle("Simpe Rolling Average - Coupled ", subtitle = "Acute v Chronic")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
     
     ggMarginal(plot, type = "histogram")
   })
   
   output$simple_ra_uncoupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'simple_ra_uncoupled') %>%
       spread(statistic, value)
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       ggtitle("Simpe Rolling Average - Uncoupled ", subtitle = "Acute v Chronic")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
     
     ggMarginal(plot, type = "histogram")
   })
   
   output$ewma_coupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'ewma_coupled') %>%
       spread(statistic, value)
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       ggtitle("EWMA Coupled ", subtitle = "Acute v Chronic")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
     
     ggMarginal(plot, type = "histogram")
   })
   
   output$ewma_uncoupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'ewma_uncoupled') %>%
       spread(statistic, value)
     
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       ggtitle("EWMA Uncoupled ", subtitle = "Acute v Chronic")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5))
     
     ggMarginal(plot, type = "histogram")
   })
   
   output$acrData <- renderDataTable({
     analysed_data() %>% 
       spread(statistic, value)
   })
}

shinyApp(ui = ui, server = server)

