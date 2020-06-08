#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(gridExtra)
library(lubridate)
library(incidence)
library(EpiEstim)
library(ggplot2)

source("get_data.R")
source("constants.R")

# Prepare data -------------
data <- get_data()

cities <- data %>%
  filter(place_type == "city")

states <- data %>%
  filter(place_type == "state")

brazil <- states %>%
  group_by(date) %>%
  summarise(
    last_available_confirmed = sum(last_available_confirmed, na.rm = TRUE),
    last_available_deaths = sum(last_available_deaths, na.rm = TRUE),
    new_confirmed = sum(new_confirmed, na.rm = TRUE),
    new_deaths = sum(new_deaths, na.rm = TRUE)
  )

rm(data)

# Prepare the selectInput entries -----------------
places <- c("BRASIL",
            levels(states$state),
            levels(as.factor(cities$city)))

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Covid-19 no Brasil"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Dados nacionais, estaduais e municipais."),
      selectInput("place",
                  "Selecione a localidade",
                  places),
      width = 3
    ),
    mainPanel(
      column(12,
      plotOutput("total_cases",
                 height = "800"),
      p("Fonte: Secretarias de Saúde das Unidades Federativas.\n
        Dados tratados por Álvaro Justen e colaboradores/", a("Brasil.IO", href="https://brasil.io/"))
    ))
  ))


# Define server logic required to draw the plots
server <- function(input, output) {
  output$total_cases <- renderPlot({
    req(input$place)
    
    df <- brazil
    
    if (nchar(as.character(input$place)) == 2) {
      df <- states %>%
        filter(state == input$place)
    } else if (input$place != "BRASIL") {
      df <- cities %>%
        filter(city == input$place)
    }
    
    # Disable warnings temporarily
    # because EpiEstim throws a lot of
    # messages and warnings by default.
    options(warn = -1)
    
    rt <- NULL
    
    if (length(df$new_confirmed[df$new_confirmed > 1]) > 1) {
      # Default usage of estimate_R generates a message
      # every time it is used, so we silence it.
      rt <- suppressMessages(estimate_R(
        as.incidence(df$new_confirmed, df$date),
        method = "parametric_si",
        config = make_config(list(
          mean_si = covid_si_mean,
          std_si = covid_si_sd
        ))
      ))
    }
    
    p1 <- ggplot(df, aes(x = date, y = last_available_confirmed)) +
      geom_col(color = "gray70") +
      geom_smooth(method = "loess",
                  formula = y ~ x,
                  size = 0.7) +
      ylab("Casos totais") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      )
    
    p2 <- ggplot(df, aes(x = date, y = last_available_deaths)) +
      geom_col(color = "gray70") +
      geom_smooth(method = "loess",
                  formula = y ~ x,
                  size = 0.7) +
      ylab("Mortos totais") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      )
    
    p3 <- ggplot(df, aes(x = date, y = new_confirmed)) +
      geom_col(color = "gray70") +
      geom_smooth(method = "loess",
                  formula = y ~ x,
                  size = 0.7) +
      ylab("Novos casos") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      )
    
    
    p4 <- ggplot(df, aes(x = date, y = new_deaths)) +
      geom_col(color = "gray70") +
      geom_smooth(method = "loess",
                  formula = y ~ x,
                  size = 0.7) +
      ylab("Novas mortes") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      )
    
    
    
    rt_plot <- ggplot() +
      labs("Not enough data for calculating R(t)")
    
    if (!is.null(rt)) {
      rt_plot <- estimate_R_plots(rt, what = "R")
    }
    
    grid.arrange(grobs = list(p1, p2, p3, p4, rt_plot),
                 layout_matrix = rbind(c(1, 2),
                                       c(3, 4),
                                       c(5, 5)))
    
    # Turn warnings on again
    options(warn = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
