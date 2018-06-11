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
library(shinycssloaders)

DEFAULT_ACUTE_PERIOD = 7
DEFAULT_CHRONIC_PERIOD = 28
ACR_METHODS = c("Simple Rolling Average Coupled" = "simple_ra_coupled", 
                "Simple Rolling Average Uncoupled" = "simple_ra_uncoupled", 
                "EWMA Coupled" = "ewma_coupled",
                "EWMA Uncoupled" = "ewma_uncoupled")

DATASETS = c("sRPE Load", "STATSports")

source('acr_calculations.R')

################
# Application UI
################
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
                   withSpinner(plotOutput("acrPlot")),
                   hr(),hr(),
                   withSpinner(plotOutput("acutePlot")),
                   hr(),hr(),
                   withSpinner(plotOutput("chronicPlot"))
          ),
          tabPanel("Correlation",
                   withSpinner(uiOutput("correlationPlots"))
          ),
          tabPanel("Raw Data", 
                   withSpinner(plotOutput("raw_dataPlot")),
                   dataTableOutput("raw_dataTable")),
          tabPanel("Information", includeHTML("references.html"))
        )
      )
   )
)


######################
# Application Server
######################
server <- function(input, output) {

  # Shiny Reactive expressions - https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
  
  ## Load the relevant dataset
  dataset <- reactive({
    
    data <- NULL
    
    if(input$dataset == "sRPE Load") {
      print("Loading from sRPE Dataset")
      data <- read_csv("data/adam_sullivan_load_data.csv")
      
    } else {
      print("Loading Statsports Dataset")
      data <- read_csv("data/statsports.csv")
      data <- data %>% rename(date = `Session Date`, athlete = `Player Display Name`)
    }
    
    data$date <- dmy(data$date)    

    data
  })
  
  # Insert a 0 workload value for every metric on days when no data present for that metric
  impute_0_on_missing_days <- function(data) {
    athlete <- data$athlete[[1]]
    all_dates <- data.frame(date = seq.Date(min(data$date), max(data$date), by = "day"), 
                            athlete = athlete)
    
    data <- merge(all_dates, data, by = c("date","athlete"), all.x = TRUE)  
    
    data[is.na(data)] <- 0
    View(data)
    data
  }
  
  ## Discover what metrics are possible for a given dataset
  metrics <- reactive({
    select_if(dataset(), is.numeric) %>% names()
  })
  
  ## Discover what athletes are possible for a given dataset
  athletes <- reactive({
    unique(dataset()$athlete)
  })
  
  ## Return the acute period as selected by the user
  acute_period <- reactive({
    input$ac_periods[1]
  })
  
  ## Return the chronic period as selected by the user
  chronic_period <- reactive({
    input$ac_periods[2]
  })
  
  ## Return the metric name as selected by the user
  metric_name <- reactive({
    input$metric_name
  })
  
  ## Filter the dataset by the athlete as selected by the user
  filtered_data <- reactive({
    dataset() %>% filter(athlete == input$athlete)
  })
  
  ## For each of the selected analysis methods
  ## 1. Conduct the analysis (using purrr::map)
  ## 2. Combine together the result data.frames from each of the analyseses (each distingushable by a new method column) 
  analysed_data <- reactive({
    input$methods %>% map(perform_analysis) %>% reduce(rbind)
  })
  
  ## Perform the relevant ACR analysis from the filtered data 
  ## will return an new data.frame with acute, chronic and acr columns added
  ##
  perform_analysis <- function(analysis_type) {
    data <- filtered_data()
    data <- impute_0_on_missing_days(data)
    acute_period <- acute_period()
    chronic_period <- chronic_period()
    
    metric <- metric_name()

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
  
  ## UI for the metrics selection (depends on the selected dataset)
  output$metricSelection <- renderUI({
    selectInput("metric_name", h4("Metric"), choices = metrics(), selected = head(metrics(), 1))
  })
  
  ## UI for the athletes selection (depends on the selected dataset)
  output$athleteSelection <- renderUI({
    selectInput("athlete", h4("Athlete"), choices = athletes(), selected = head(athletes(), 1))
  })
  
  
  ## The Acute/Chronic ratio plot. Each analysis represented by a different color
   output$acrPlot <- renderPlot({
     data <- analysed_data() %>% filter(statistic == 'acr')
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
  
   ## The Acute workload plot. Each analysis represented by a different color 
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
   
   ## The Chronic workload plot. Each analysis represented by a different color 
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
   
   ## Draw the relevant number of correlation plots (depends on which analysis methods chosen)
   output$correlationPlots <- renderUI({
     
     analysis_methods <- input$methods
     
     ### Loop over the analysis methods chosen and create the relevant plot
     plot_output_list <- lapply(analysis_methods, function(analysis_method) {
       
       ## Each plot must have a unique name
       plotname <- paste("plot", analysis_method, sep=" - ")
       
       plot_output_object <- plotOutput(plotname)
       
       plot_output_object <- renderPlot({
         
         ## Filter the data by analysis method
         data <- analysed_data() %>% 
           filter(method == analysis_method) %>%
           spread(statistic, value)

         ## Correlation plot for the particular analysis method         
         plot <- ggplot(data, aes(acute, chronic, color = acr)) + 
           ggtitle(analysis_method, subtitle = "Acute v Chronic")  +
           geom_point() +
           geom_smooth(method = "lm", se = FALSE) +
           stat_cor(method = "pearson") + 
           theme_minimal() +
           scale_colour_gradient2(guide = FALSE, high = "red", low = "yellow", mid = "white") +
           theme(plot.title = element_text(hjust = 0.5, vjust = -0.5),
                 plot.subtitle = element_text(hjust = 0.5, vjust = -0.5)) +
           theme(title =element_text(size=12, face='bold'))
         
         
         ## Add the chronic distribution as a marginal plot (using ggpub)
         ggMarginal(plot, type = "histogram", margins = c("y"), alpha = 0.75)
       })
       
       ## Add some hr() HTML elements for spacing (should be properly done in CSS)
       list(plot_output_object, hr(), hr())
     })
     
     do.call(tagList, plot_output_list)
     
     ## Return the full list of plots and HTML elements
     return(plot_output_list)
   })
   
   ## Raw data plot, alternative weeks are colored to show weekly differences
   output$raw_dataPlot <- renderPlot({
     data <- analysed_data() %>% spread(statistic, value)
     data$week <- week(data$date) %% 2
     
     ggplot(data, aes_string("date", paste0("`",metric_name(),"`"), fill = "week")) + 
       geom_bar(stat = "identity") +
       theme_minimal() +
       guides(fill = FALSE)
   })
   
   ## Raw data table
   output$raw_dataTable <- renderDataTable({
     analysed_data() %>% 
       spread(statistic, value)
   })
}

shinyApp(ui = ui, server = server)

