
#libraries to include
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape2)
library(scales)
library(dplyr)
library(plotly)
library(shinyWidgets)

# load any processed data here
load("rdata/allPopularCarriers.RData")

# start up the gui
ui <- dashboardPage(
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Airlines", icon = icon("plane", lib = "font-awesome"), tabName = "topCarriers"),
      menuItem("Top Airports", tabName = "bart", icon = icon("dashboard")),
      menuItem("Top Airports 12 mo.", tabName = "bart2", icon = icon("dashboard")),
      menuItem("Hourly Total", icon = icon("hourglass", lib = "font-awesome"), tabName = "hourlytotal"),
      menuItem("Weekly Total", icon = icon("calendar", lib = "font-awesome"), tabName = "arrivalDepartureDaily"),
      menuItem("Delays", icon = icon("hourglass", lib = "font-awesome"), tabName = "delays"),
      
      #get month
      selectInput("month-select", label = "Month", list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
                  selected = 1),
      
      #get day of week
      selectInput("week-select", label = "Day of Week", list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7, "All" = 8), 
                  selected = 8),

      #get specific date
      dateInput("date-select", label = h5("Specific Date")),
      
      #change between 12/24 hours time formats
      switchInput(inputId = "time",label = "24 time format", value = TRUE),
      
      #info
      menuItem("Info", tabName = "info", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "bart",
              fluidRow( box(title = "Flights from/to Chicago O'Hare International", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable1")
              ),
              box(title = "Flights from/to Chicago O'Hare International", solidHeader = TRUE, width = 10,
                  div(plotlyOutput("bartChart1"))
              )),
              box(title = "Flights from/to Chicago Midway International", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable2")
              ),
              box(title = "Flights from/to Chicago Midway International", solidHeader = TRUE, width = 10,
                  div(plotlyOutput("bartChart2"))
              )
              
      ),
      tabItem(tabName = "bart2",
              fluidRow(
              box(title = "Heatmap", solidHeader = TRUE, width = 10,
                  div(plotlyOutput("top15Airports12months"))
              )
              )
              
      ),
      
      
      tabItem(tabName = "delays",
              fluidRow(
                radioGroupButtons(
                  inputId = "delayButtons", label = "Types of Delay :",
                  choices = c("Carrier", "Weather", "National Air System", "Security", "Late Aircraft"),
                  justified = TRUE, status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                ),
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("delayGraph"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                    DT::dataTableOutput("totalselectedDataPercentageTable")
                ),
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("yearlyDelaysGraph"))
                )
                
                
                
              )
      ),
      
      tabItem(tabName = "hourlytotal",
              fluidRow(
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("hourlyGraph"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                    DT::dataTableOutput("totalselectedDataTable")
                ),
                tabBox(
                  title = "Yearly",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Arrivals", div(plotlyOutput("hourlyYearGraphArr"))),
                  tabPanel("Departures", div(plotlyOutput("hourlyYearGraphDep")))
                )
              ),
              
              
              
              
              
              # My weekday stuff. Trying to figure out best way to display it. -Vijay
              h4("Weekday Information in December 2017"),
              fluidRow(
                box(title = "Departures and Arrivals by Weekday Graph", solidHeader = TRUE, status = "primary", width = 12,
                    div(plotlyOutput("weekdayGraph"))
                )
              ),
              fluidRow(
                box(title = "Departures and Arrivals by Weekday Table", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("weekdayTable")
                )
              )
      ),
      tabItem(tabName = "topCarriers",
              # Data across chosen month (from month dropdown)
              fluidRow(
                box(title = "Month View - Top Airlines Total Departures/Arrivals", solidHeader = TRUE, status = "primary", width = 6,
                  tabBox(
                    # The id lets us use input$monthText on the server to find the current tab
                    title = textOutput('monthText', inline = TRUE),
                    id = "monthTopCarriers", height = "100%", width = "100%",
                    tabPanel("Graph", div(plotlyOutput("popularGraph"))),
                    tabPanel("Table", div(DT::dataTableOutput("topCarriersTable")))
                  )
                ),
                box(title = "Year View - Top Airlines Total Departures/Arrivals", solidHeader = TRUE, status = "primary", width = 6,
                    tabBox(
                      title = "January - December 2017",
                      id = "yearTopCarriers", height = "100%", width = "100%",
                      tabPanel("Graph", div(plotlyOutput("allMonthsPopularGraph"))),
                      tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")))
                    )
                )
              ),
              
              # User selects CARRIER from list of available airlines
              # TODO: instead of having dropdown sorted by popularity have it sorted alphabetically
              fluidRow(
                box(title = "Departures/Arrivals for Selected Airline", solidHeader = TRUE, status = "primary", width = 12,
                    selectInput("airline-dropdown", "Airline:", choices = as.character(allPopularCarriers$CARRIER)),
                    tabBox( # TODO: 24 hour breakdown of chosen carrier
                      title = "24 Hour Breakdown of [CARRIER]",
                      id = "twentyFourTopCarriers", height = "100%", width = "100%"#,
                      #tabPanel("Graph", div(plotlyOutput("specificCarrierPlot")))#,
                      #tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")))
                    ),
                    tabBox( # TODO: Year breakdown of chosen carrier
                      title = "January - December 2017",
                      id = "yearChosenTopCarrier", height = "100%", width = "100%"#,
                      #tabPanel("Graph", div(plotlyOutput("allMonthsPopularGraph")))#,
                      #tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")))
                    )
                )
              )
      ),
      tabItem(tabName = "info",
              h1("Aeroplane Visualization"),
              h2("Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae"),
              h2("Project 2 for CS 424 Spring 2018 UIC"),
              h4("If graphs are 'glitchy' resize your window."),
              h4("Libraries used:"),
              h4("shinydashboard"),
              h4("ggplot2"),
              h4("lubridate"),
              h4("DT"),
              h4("jpeg"),
              h4("grid"),
              h4("leaflet"),
              h4("reshape2"),
              h4("scales"),
              h4("dplyr"),
              h4("plotly"),
              h4("shinyWidgets")
      )
    )
    
  )
)

