simple_rolling_average_coupled <- function(data, metric, acute_period, chronic_period) {
  data %>% 
    mutate(
      acute = rollapply(data[,metric], acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
      chronic = rollapply(data[,metric], chronic_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
      acr = acute / chronic) %>%
    gather(statistic, value, acute, chronic, acr)
}

simple_rolling_average_uncoupled <- function(data, metric, acute_period, chronic_period) {
  data %>% 
    mutate(
      acute = rollapply(data[,metric], acute_period, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
      chronic = rollapply(data[,metric], c(list(-acute_period:-(chronic_period+acute_period)), -chronic_period), FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
      acr = acute / chronic) %>%
    gather(statistic, value, acute, chronic, acr)
}

ewma_coupled <- function(data, metric, acute_period, chronic_period) {
  acute_alpha <- 2/(acute_period+1)
  chronic_alpha <- 2/(chronic_period+1)
  
  data$acute <- NA
  data$chronic <- NA
  
  data$acute[1] <- data[[metric]][1]
  data$chronic[1] <- data[[metric]][1]
  
  for(i in (2:dim(data)[1])){
    data$acute[i] <- data[[metric]][i] * acute_alpha + (1-acute_alpha) * data$acute[i-1]
    data$chronic[i] <- data[[metric]][i] * chronic_alpha + (1-chronic_alpha) * data$chronic[i-1]
  }
  
  data %>%
    mutate(
      acr = acute / chronic) %>%
    gather(statistic, value, acute, chronic, acr)
}

ewma_uncoupled <- function(data, metric, acute_period, chronic_period) {
  acute_alpha <- 2/(acute_period+1)
  chronic_alpha <- 2/(chronic_period+1)
  
  data$acute <- NA
  data$chronic <- NA
  
  data$acute[1] <- data[[metric]][1]
  data$chronic[1:7] <- mean(data[[metric]][1:7])
  
  for(i in (2:dim(data)[1])){
    data$acute[i] <- data[[metric]][i] * acute_alpha + (1-acute_alpha) * data$acute[i-1]
    if(i > 7) {
      data$chronic[i] <- data[[metric]][i-7] * chronic_alpha + (1-chronic_alpha) * data$chronic[i-7]
    }
  }
  
  data %>%
    mutate(
      acr = acute / chronic) %>%
    gather(statistic, value, acute, chronic, acr)
}