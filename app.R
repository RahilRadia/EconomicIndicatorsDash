library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)
library(shinydashboard)
library(plotly)

data <- read_csv("./econ_data.csv")
data <- pivot_longer(data, cols = -c(Country_Name, Country_Code, Indicator_Name, Indicator_Code), names_to = "Year", values_to = "Value")

ui <- dashboardPage(
  dashboardHeader(
      title = span("World Bank Economic Indicators",
        tags$a(href = "https://github.com/RahilRadia/EconomicIndicatorsDash", 
                      target = "_blank", "Github Repository", style = "color: blue; font-size: 14px")),
      titleWidth = 440
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ),
    selectInput("indicator", "Select Indicator", choices = unique(data$Indicator_Name)),
    selectInput("countries", "Select Countries", choices = unique(data$Country_Name), multiple = TRUE)
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #89b87d;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #89b87d;
        }
        .skin-blue .main-header .navbar {
          background-color: #9cd18e;
        }
        .skin-blue .main-sidebar {
          background-color: #9cd18e;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #497349;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #555;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #9cd18e;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          color: #fff;
        }
         body, .content-wrapper, .right-side {
          background-color: #f0f4f0;
        }
        .box.box-primary {
          border-top-color: #9cd18e;
        }
        .box.box-primary > .box-header {
          background-color: #9cd18e;
          color: #fff;
        }
        .box.box-primary > .box-header h3.box-title {
          color: #fff;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Economic Indicator Trends", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("linePlot")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlotly({
    req(input$indicator, input$countries)
    
    selected_data <- data %>%
      filter(Indicator_Name == input$indicator, Country_Name %in% input$countries)
    
    if (nrow(selected_data) == 0) {
      return(NULL)
    }
    
    p <- ggplot(selected_data, aes(x = as.numeric(Year), y = Value, color = Country_Name)) + 
      geom_line() +
      labs(title = paste("Trends for" ,unique(selected_data$Indicator_Name)), x = "Years", y = unique(selected_data$Indicator_Name), color = "Country") +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "#89b87d"),
        panel.grid.minor = element_line(color = "#9cd18e"),
        legend.background = element_rect(fill = "#ffffff", color = NA)
      ) 
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
