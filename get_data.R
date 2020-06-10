library(dplyr)
library(readr)
library(lubridate)
library(purrr)


get_data <- function() {
  con <-
    gzcon(url(
      "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
    ))
  
  txt <- readLines(con)
  close(con)
  data <- dplyr::tibble(read.csv(textConnection(txt)))
  data$date <- ymd(data$date)
  
  # Some data points show negative new deaths and negative new cases
  data$new_confirmed[which(data$new_confirmed < 0)] <- 0
  data$new_deaths[which(data$new_deaths < 0)] <- 0
  data$city <- as.character(data$city)
  
  data <- data %>% split(., .$place_type)
  
  data$city <- data$city %>%
    mutate(key = paste(city, state, sep = " - "))
  
  data$state <- data$state %>%
    mutate(key = state)
  
  # Remove cities with zero confirmed cases
  data$city <- data$city %>%
    filter(sum(new_confirmed) > 0)
  
  data %>%
    map(
      ~ select(
        .,
        key,
        date,
        city,
        state,
        place_type,
        last_available_confirmed,
        last_available_deaths,
        new_confirmed,
        new_deaths
      )
    )
  
  data
  
}
