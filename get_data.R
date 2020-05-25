library(dplyr)


get_data <- function() {
  con <- gzcon(url("https://data.brasil.io/dataset/covid19/caso_full.csv.gz"))
  
  txt <- readLines(con)
  data <- dplyr::tibble(read.csv(textConnection(txt)))
  data$date <- as.Date(data$date)
  
  # Some data points show negative new deaths and negative new cases
  data$new_confirmed[which(data$new_confirmed < 0)] <- 0
  data$new_deaths[which(data$new_deaths < 0)] <- 0
  data$city <- as.character(data$city)
  
  # Remove cities with zero confirmed cases
  data <- data %>%
    group_by(city) %>% 
    filter(sum(new_confirmed) > 0) %>%
    select(date, city, state, place_type, last_available_confirmed, last_available_deaths, new_confirmed, new_deaths)

  data

}
# con <- gzcon(url("https://data.brasil.io/dataset/covid19/caso_full.csv.gz"))

# txt <- readLines(con)
# covid <- dplyr::tibble(read.csv(textConnection(txt)))
# covid$date <- as.Date(covid$date)
# 
# # Some data show negative new deaths and negative new cases
# covid$new_confirmed[which(covid$new_confirmed < 0)] <- 0
# covid$new_deaths[which(covid$new_deaths < 0)] <- 0
# 
# covid <- covid %>%
#   select(date, city, state, place_type, last_available_confirmed, last_available_deaths, new_confirmed, new_deaths)
# 
# saveRDS(covid, "covid.rds")

