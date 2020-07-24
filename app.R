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
library(incidence)
library(EpiEstim)
library(ggplot2)
library(shinythemes)
library(dygraphs)
library(xts)

source("get_data.R")

# Prepare data ----------------------------------------------------------------
data <- get_data()
# data <- readRDS("sample-data.rds")
cities <- data$city
states <- data$state

brazil <- states %>%
  group_by(date) %>%
  summarise(
    last_available_confirmed = sum(last_available_confirmed, na.rm = TRUE),
    last_available_deaths = sum(last_available_deaths, na.rm = TRUE),
    new_confirmed = sum(new_confirmed, na.rm = TRUE),
    new_deaths = sum(new_deaths, na.rm = TRUE)
  )

# Constants -------------------------------------------------------------------

# COVID-19 data from mrc/imperial college
# can be found at: https://mrc-ide.github.io/covid19-short-term-forecasts/
covid_si_mean <- 6.48
covid_si_sd <- 3.83

# Prepare the selectInput entries ---------------------------------------------
places <- c("BRASIL",
            levels(states$key),
            levels(as.factor(cities$key)))

# Define UI for application that draws a histogram
ui <- navbarPage(
  "COVID-19 Brasil",
  theme = shinytheme("flatly"),
  tabPanel("Dashboard",
           fluidPage(
             # Application title
             sidebarLayout(
               sidebarPanel(
                 helpText("Dados nacionais, estaduais e municipais."),
                 selectInput("place",
                             "Selecione a localidade",
                             places),
                 width = 3
               ),
               mainPanel(
                 column(
                   12,
                   textOutput("date"),
                   dygraphOutput("total_cases"),
                   dygraphOutput("new_cases"),
                   dygraphOutput("total_deaths"),
                   dygraphOutput("new_deaths"),
                   dygraphOutput("rt"),
                   p(
                     "Fonte: Secretarias de Saúde das Unidades Federativas.\n
        Dados tratados por Álvaro Justen e colaboradores/",
                     a("Brasil.IO", href = "https://brasil.io/")
                   )
                 )
               )
             )
           )),
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
        href = "https://github.com/samirarman/covid-brasil-dashboard"),
      p("Autor: Samir Arman"),
      p("samir.arman@gmail.com")
    )
  )
)


# Define server logic required to draw the plots ------------------------------
server <- function(input, output) {
  # Make data ------------------------------
  filter_data <- reactive({
    req(input$place)
    
    df <- brazil
    
    if (nchar(as.character(input$place)) == 2) {
      df <- states %>%
        filter(state == input$place)
    } else if (input$place != "BRASIL") {
      df <- cities %>%
        filter(key == input$place)
    }
    
    df
  })
  
  # Estimate R -----------------------------
  get_est_R <- reactive({
    # Disable warnings temporarily
    # because EpiEstim throws a lot of
    # messages and warnings by default.
    options(warn = -1)
    
    df <- filter_data()
    
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
    
    read.zoo(rt$dates[8:length(rt$dates)] %>%
               cbind(rt$R %>%
                       select(c(3, 5, 11))))
  })
  
  # Make plots -----------------------------
  make_plot <- function(var, title) {
    data <- filter_data()
    
    loess_fit <-
      loess(paste0(var,  " ~ ",  "as.integer(date)"), data = data) %>%
      predict(data, se = T)

    # Trick to add a new row with series value = 0 in order to 
    # make the chart look better
    series <- xts(c(data[[var]],0) , c(brazil$date, brazil$date[length(brazil$date)] + 1))
    
    trend <- xts(loess_fit$fit, data$date)
    upper <- xts(trend + qnorm(0.975) * loess_fit$se.fit, data$date)
    lower <- xts(trend + qnorm(0.025) * loess_fit$se.fit, data$date)
    
    chart_data <- cbind(series, trend, upper, lower)
    names(chart_data) <- c("series", "trend", "upper", "lower")
    
    dygraph(chart_data, main = title, group = "all") %>%
      dySeries('trend', color = "blue") %>%
      dySeries('upper', color = "gray80") %>%
      dySeries('lower', color = "gray80") %>%
      dySeries('series', stepPlot = T, fillGraph = T, color = "gray") %>%
      dyRangeSelector()
    
  }
  
  output$total_cases <- renderDygraph({
    make_plot("last_available_confirmed", "Total de casos")
    
  })
  
  
  output$new_cases <- renderDygraph({
    make_plot("new_confirmed", "Novos casos")
  })
  
  output$total_deaths <- renderDygraph({
    make_plot("last_available_confirmed", "Total de óbitos")
  })
  
  output$new_deaths <- renderDygraph({
    make_plot("new_deaths", "Novos óbitos")
  })
  
  output$rt <- renderDygraph({
    need(!is.null(ts),
         "Sem dados  suficientes para calcular
         a taxa de reprodução")
    ts <- get_est_R()
    dygraph(ts, main = "Taxa de reprodução", group = "all") %>%
      dySeries("Quantile.0.975(R)", color = "gray") %>%
      dySeries("Mean(R)", color = "red") %>%
      dySeries("Quantile.0.025(R)", color = "gray") %>%
      dyRangeSelector()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
