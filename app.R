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
library(plotly)

source("get_data.R")
source("constants.R")

# Prepare data ----------------------------------------------------------------
data <- get_data()

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
                   plotlyOutput("total_cases"),
                   plotlyOutput("new_cases"),
                   plotlyOutput("total_deaths"),
                   plotlyOutput("new_deaths"),
                   plotlyOutput("rt"),
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
  get_correct_data <- reactive({
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
    
    rt$dates[8:length(rt$dates)] %>%
      cbind(rt$R %>%
              select(c(5, 8, 11))) %>%
      rename(
        date = !!".",
        quant_025 = !!"Quantile.0.025(R)",
        median = !!"Median(R)",
        quant_975 = !!"Quantile.0.975(R)"
      )
    
  })
  
  # Make plots -----------------------------
  make_plot <- function(var, title) {
    data <- get_correct_data()
    
    col <- enquo(var)
    
    
    ggplotly(
      ggplot(data, aes(x = date, y = !!col)) +
        geom_col() +
        geom_smooth(
          method = "loess",
          formula = y ~ x,
          size = 0.5
        ) +
        labs(title = title) +
        ylab("") +
        xlab("") +
        theme_bw()
    )
  }
  
  output$total_cases <- renderPlotly({
    make_plot(last_available_confirmed, "Total de casos")
  })
  
  output$total_deaths <- renderPlotly({
    make_plot(last_available_deaths, "Total de óbitos")
  })
  
  output$new_cases <- renderPlotly({
    make_plot(new_confirmed, "Novos casos")
  })
  
  output$new_deaths <- renderPlotly({
    make_plot(new_deaths, "Novos óbitos")
  })
  
  output$rt <- renderPlotly({
    need(!is.null(data),
         "Sem dados suficientes para calcular
         a taxa de reprodução.")
    data <- get_est_R()
    
    ggplotly(
      ggplot(data, aes(x = date)) +
        geom_ribbon(
          aes(ymin = quant_025, ymax = quant_975),
          fill = "gray",
          size = 0.4
        ) +
        geom_line(aes(y = median), color = "#3366FF", size = 0.4) +
        theme_bw() +
        labs(title = "Taxa de reprodução estimada - R(t)") +
        xlab("") +
        ylab("")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
