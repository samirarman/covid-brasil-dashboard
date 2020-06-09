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
library(shinythemes)

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


# Set ggplot option
ggplot2::theme_set(theme_bw())

# Prepare the selectInput entries -----------------
places <- c(
  "BRASIL",
  levels(states$state),
  levels(as.factor(cities$city))
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "COVID-19 Brasil",
  theme = shinytheme("flatly"),
  tabPanel(
    "Dashboard",
    fluidPage(
      # Application title
      sidebarLayout(
        sidebarPanel(
          helpText("Dados nacionais, estaduais e municipais."),
          selectInput(
            "place",
            "Selecione a localidade",
            places
          ),
          width = 3
        ),
        mainPanel(
          column(
            12,
            textOutput("date"),
            plotOutput("total_cases"),
            plotOutput("new_cases"),
            plotOutput("total_deaths"),
            plotOutput("new_deaths"),
            plotOutput("rt"),
            p(
              "Fonte: Secretarias de Saúde das Unidades Federativas.\n
        Dados tratados por Álvaro Justen e colaboradores/",
              a("Brasil.IO", href = "https://brasil.io/")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Sobre",
    p(
      "Este projeto é uma visualização rápida e resumida
      dos dados das Secretárias de Saúde obtidos através do projeto ",
      a("Brasil.IO", href = "https://brasil.io/"),
      " e foi realizado
        com o objetivo de aprendizado e de obter informações rápidas sobre
        a propagação da COVID-19 nos municípios."
    ),

    p(
      "Incluímos uma estimativa da taxa de reprodução atual de cada localidade,
      de modo a dar uma ideia da velocidade de propagação da pandemia,
      sem propósito científico algum. Este último gráfico
      só é mostrado caso haja dados suficientes."
    ),
    p(
      "Se uma cidade não está listada, 
      significa que não possui casos reportados."
    ),
    p(
      "O código-fonte utilizado está disponível em ",
      a("github.com/samirarman/covid-brasil-dashboard",
        href = "https://github.com/samirarman/covid-brasil-dashboard"
      ),
      p("Autor: Samir Arman"),
      p("samir.arman@gmail.com")
    )
  )
)


# Define server logic required to draw the plots -------
server <- function(input, output) {
  get_correct_data <- reactive({
    req(input$place)

    df <- brazil

    if (nchar(as.character(input$place)) == 2) {
      df <- states %>%
        filter(state == input$place)
    } else if (input$place != "BRASIL") {
      df <- cities %>%
        filter(city == input$place)
    }

    return(df)
  })

  get_est_R <- reactive({

    # Disable warnings temporarily
    # because EpiEstim throws a lot of
    # messages and warnings by default.
    options(warn = -1)

    df <- get_correct_data()

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
    options(warn = 1)
    return(rt)
  })

  output$total_cases <- renderPlot({
    df <- get_correct_data()

    ggplot(df, aes(x = date, y = last_available_confirmed)) +
      geom_col(color = "gray70") +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        size = 0.7
      ) +
      ylab("Casos totais") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x) {
          format(x, scientific = FALSE)
        }
      )
  })

  output$total_deaths <- renderPlot({
    df <- get_correct_data()

    ggplot(df, aes(x = date, y = last_available_deaths)) +
      geom_col(color = "gray70") +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        size = 0.7
      ) +
      ylab("Mortos totais") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x) {
          format(x, scientific = FALSE)
        }
      )
  })

  output$new_cases <- renderPlot({
    df <- get_correct_data()

    ggplot(df, aes(x = date, y = new_confirmed)) +
      geom_col(color = "gray70") +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        size = 0.7
      ) +
      ylab("Novos casos") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x) {
          format(x, scientific = FALSE)
        }
      )
  })

  output$new_deaths <- renderPlot({
    df <- get_correct_data()

    ggplot(df, aes(x = date, y = new_deaths)) +
      geom_col(color = "gray70") +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        size = 0.7
      ) +
      ylab("Novas mortes") +
      xlab("Data") +
      scale_y_continuous(
        labels = function(x) {
          format(x, scientific = FALSE)
        }
      )
  })

  output$rt <- renderPlot({
    rt <- get_est_R()

    rt_plot <- ggplot() +
      labs("Not enough data for calculating R(t)")

    if (!is.null(rt)) {
      rt_plot <- estimate_R_plots(rt, what = "R")
    }

    return(rt_plot)
  })

  output$date <- renderText({
    df <- get_correct_data()
    paste("Última atualização: ",
      format(max(df$date), "%d/%m/%Y"),
      sep = " "
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
