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

DATASETS = c("sRPE Load (AS)", "STATSports")

source('acr_calculations.R')

# Application UI
ui <- fluidPage(theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Acute/Chronic Workload Ratio"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("dataset", h4("Dataset"), choices = DATASETS, selected = head(DATASETS, 1)),
        uiOutput("metricSelection"),
        uiOutput("athleteSelection"),
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
          ),
          tabPanel("Raw Data", 
                   plotOutput("raw_dataPlot"),
                   dataTableOutput("raw_dataTable")),
          tabPanel("References", p("List references here"))
        )
      )
   )
)

server <- function(input, output) {

  dataset <- reactive({
    
    data <- NULL
    
    if(input$dataset == "sRPE Load (AS)") {
      print("Loading from SRPE Dataset")
      data <- read_csv("data/adam_sullivan_load_data.csv")
    } else {
      print("Loading Statsports Dataset")
      data <- read_csv("data/statsports.csv")
      data <- data %>% rename(date = `Session Date`, athlete = `Player Display Name`)
    }
    
    data$date <- dmy(data$date)    

    data
  })

  metrics <- reactive({
    metric_names <- select_if(dataset(), is.numeric) %>% names()
    
#    if(input$dataset == "sRPE Load (AS)") {
 #     print("Using SRPE Metric Names")
  #    metric_names <- names(dataset())[!names(dataset()) %in% c("date", "athlete")]
   # } else {
  #    print("Using Statsports Metric Names")
  #    metric_names <- names(dataset())[!names(dataset()) %in% c("Player Display Name", "Session Date")]
  #  }
    
    print(paste("Using metric names: ", metric_names))
    
    metric_names
  })
  
  athletes <- reactive({
    unique(dataset()$athlete)
  })
  
  acute_period <- reactive({
    input$ac_periods[1]
  })
  
  chronic_period <- reactive({
    input$ac_periods[2]
  })
  
  filtered_data <- reactive({
    dataset() %>% filter(athlete == input$athlete)
  })
  
  metric_name <- reactive({
    input$metric_name
  })
  
  perform_analysis <- function(x) {
    
    analysis_type <- x
    data <- filtered_data()
    acute_period <- acute_period()
    chronic_period <- chronic_period()
    
    metric <- metric_name()

    print("performing analysis")
    if(analysis_type == "simple_ra_coupled") {
      simple_rolling_average_coupled(data, metric, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else if(analysis_type == "simple_ra_uncoupled") {
      simple_rolling_average_uncoupled(data, metric, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else if (analysis_type == "ewma_coupled") {
      ewma_coupled(data, metric, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } else {
      ewma_uncoupled(data, metric, acute_period, chronic_period) %>% mutate(method = analysis_type)
    } 
  }
  
  analysed_data <- reactive({
    input$methods %>% map(perform_analysis) %>% reduce(rbind)
  })

  output$metricSelection <- renderUI({
    selectInput("metric_name", h4("Metric"), choices = metrics(), selected = head(metrics(), 1))
  })
  
  output$athleteSelection <- renderUI({
    selectInput("athlete", h4("Athlete"), choices = athletes(), selected = head(athletes(), 1))
  })
  
   output$acrPlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'acr')
     print(glimpse(data))
     ggplot(data, aes(date, value, color = method)) + 
       geom_line() + 
       xlab(NULL) + ylab(NULL)+
       theme_minimal() +
       ggtitle("Acute/Chronic Ratio")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(legend.position="bottom") +
       theme(legend.title=element_blank()) +
       theme(title =element_text(size=12, face='bold'))
   })
   
   output$acutePlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'acute')
     ggplot(data, aes(date, value, color = method)) + 
       geom_line() + 
       xlab(NULL) + ylab(NULL)+
       theme_minimal() +
       ggtitle("Acute Workload")  +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(legend.position="bottom") +
       theme(legend.title=element_blank()) +
       theme(title =element_text(size=12, face='bold'))
   })
   
   output$chronicPlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'chronic')
     ggplot(data, aes(date, value, color = method)) + 
       ggtitle("Chronic Workload")  +
       geom_line() + 
       xlab(NULL) + ylab(NULL)+
       theme_minimal() +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(legend.position="bottom") +
       theme(legend.title=element_blank()) +
       theme(title =element_text(size=12, face='bold'))
   })
   
   output$simple_ra_coupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'simple_ra_coupled') %>%
       spread(statistic, value)
     
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       ggtitle("Simpe Rolling Average - Coupled ", subtitle = "Acute v Chronic")  +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       scale_colour_gradient2(guide = FALSE, high = "red", low = "yellow", mid = "white") +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(title =element_text(size=12, face='bold'))
     
     ggMarginal(plot, type = "histogram", margins = c("y"), alpha = 0.75)
   })
   
   output$simple_ra_uncoupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'simple_ra_uncoupled') %>%
       spread(statistic, value)
     
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       ggtitle("Simpe Rolling Average - Uncoupled ", subtitle = "Acute v Chronic")  +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       scale_colour_gradient2(guide = FALSE, high = "red", low = "yellow", mid = "white") +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(title =element_text(size=12, face='bold'))
     
     ggMarginal(plot, type = "histogram", margins = c("y"), alpha = 0.75)
   })
   
   output$ewma_coupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'ewma_coupled') %>%
       spread(statistic, value)
     
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       ggtitle("EWMA Coupled ", subtitle = "Acute v Chronic")  +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       scale_colour_gradient2(guide = FALSE, high = "red", low = "yellow", mid = "white") +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(title =element_text(size=12, face='bold'))
     
     ggMarginal(plot, type = "histogram", margins = c("y"), alpha = 0.75)
   })
   
   output$ewma_uncoupled_correlationPlot <- renderPlot({
     data <- analysed_data() %>% 
       filter(method == 'ewma_uncoupled') %>%
       spread(statistic, value)
     
     plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
       ggtitle("EWMA Uncoupled ", subtitle = "Acute v Chronic")  +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_cor(method = "pearson") + 
       theme_minimal() +
       scale_colour_gradient2(guide = FALSE, high = "red", low = "yellow", mid = "white") +
       theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
             plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
       theme(title =element_text(size=12, face='bold'))
     
     ggMarginal(plot, type = "histogram", margins = c("y"), alpha = 0.75)
   })
   
   output$raw_dataPlot <- renderPlot({
     data <- analysed_data() %>% spread(statistic, value)
     data$week <- week(data$date) %% 2
     
     
     ggplot(data, aes_string("date", paste0("`",metric_name(),"`"), fill = "week")) + 
       geom_bar(stat = "identity") +
       theme_minimal() +
       guides(fill = FALSE)
   })
   
   output$raw_dataTable <- renderDataTable({
     analysed_data() %>% 
       spread(statistic, value)
   })
}

shinyApp(ui = ui, server = server)

