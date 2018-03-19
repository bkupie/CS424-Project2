
#libraries to include
library(shiny)
library(shinyjs)
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
library(shinycssloaders)

# load any processed data here
load("rdata/allPopularCarriers.RData")
load("rdata/top50Airports.RData")
load("rdata/interesting.RData")

# start up the gui
ui <- dashboardPage(
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Airlines", icon = icon("plane", lib = "font-awesome"), tabName = "topCarriers"),
      menuItem("Top Airports", tabName = "TopAirport", icon = icon("dashboard")),
      menuItem("Hourly Total", icon = icon("hourglass", lib = "font-awesome"), tabName = "hourlytotal"),
      menuItem("Weekly Total", icon = icon("calendar", lib = "font-awesome"), tabName = "weeklyTotal"),
      menuItem("Delays", icon = icon("hourglass", lib = "font-awesome"), tabName = "delays"),
      menuItem("Map", icon = icon("fa fa-map-o", lib = "font-awesome"), tabName = "map"),
      menuItem("Interesting days", icon = icon("calendar", lib = "font-awesome"), tabName = "int-days"),
      
      #get month
      selectInput("month-select", label = "Month", list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
                  selected = 1),
      
      #change between 12/24 hours time formats
      materialSwitch(inputId = "time", label = "24 Time Format", status = "primary", right = TRUE, value = TRUE),

      #info
      menuItem("Info", tabName = "info", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "TopAirport",
              fluidRow( box(title = "Amount of flights from/to Chicago O'Hare International in a month", solidHeader = TRUE, status = "primary", width = 6,
                  dataTableOutput("top15Table1") %>% withSpinner(color="#0dc5c1")
              ),
              box(title = "Amount of flights from/to Chicago Midway International in a month", solidHeader = TRUE, status = "primary", width = 6,
                  dataTableOutput("top15Table2") %>% withSpinner(color="#0dc5c1")
              )),
              fluidRow(
              box(title = "Amount of flights from/to Chicago O'Hare International in a month", solidHeader = TRUE, width = 6,
                  div(plotlyOutput("top15Chart1") %>% withSpinner(color="#0dc5c1"))
              ),
              box(title = "Amount of flights from/to Chicago Midway International in a month", solidHeader = TRUE, width = 6,
                  div(plotlyOutput("top15Chart2") %>% withSpinner(color="#0dc5c1"))
              )),
              fluidRow(
                box(selectInput("airport-top50-dropdown", "Destination Airport:", choices = as.character(top50Airports$Airport),selected = "Chicago O'Hare International"),
                box(title = "Flights from/to selected airport per hour", solidHeader = TRUE, width = 6,
                    plotlyOutput("top50") %>% withSpinner(color="#0dc5c1")),
                box(title = "Flights from/to selected airport per month", solidHeader = TRUE, width = 6,
                    plotlyOutput("top50year") %>% withSpinner(color="#0dc5c1")),width = 12)
              ),
              fluidRow(
                box(title = "Top airports for 12 months", solidHeader = TRUE, width = 12,
                    div(plotlyOutput("top15Airports12months") %>% withSpinner(color="#0dc5c1"))
                )
             )
              
      ),
      tabItem(tabName = "map",
              tags$style(type = "text/css", "#FLightMap {height: calc(100vh - 80px) !important;}"),
              fluidRow(
                materialSwitch(inputId = "exclude", label = "Exclude Illinois data", status = "primary",inline = TRUE, right = TRUE, value = FALSE),
                box(title = "", solidHeader = TRUE, height = 400,width = 400,
                    plotlyOutput("FLightMap") %>% withSpinner(color="#0dc5c1")
              ))),
      
      
      tabItem(tabName = "delays",
              fluidRow(
                box(width=12,
                    useShinyjs(),
                    checkboxInput("checkbox", "Pick a specific Date", FALSE),
                    div(dateInput("dateHourlyDelays", label = h3("Specific Date"), value = "2017-01-01"))
                ),
                radioGroupButtons(
                  inputId = "delayButtons", label = "Types of Delay :",
                  choices = c("Carrier", "Weather", "National Air System", "Security", "Late Aircraft"),
                  justified = TRUE, status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                ),
                tabBox(
                  title = "Hourly Delays",
                  width = 12,
                  id = "tabset3", height = "1000px",
                  tabPanel("O'Hare and Midway",
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(plotlyOutput("delayGraph", height = 900))
                           ),
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(DT::dataTableOutput("totalselectedDataPercentageTable", height= 900))
                           )),
                  tabPanel("O'Hare", 
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(plotlyOutput("delayGraphORD", height = 900))
                           ),
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(DT::dataTableOutput("totalselectedDataPercentageTableORD", height= 900))
                           )),
                  tabPanel("Midway", 
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(plotlyOutput("delayGraphMID", height = 900))
                           ),
                           box(status = "primary", solidHeader = TRUE, width = 6,
                               div(DT::dataTableOutput("totalselectedDataPercentageTableMID", height= 900))
                           )
                           )
                ),
                
                box(status = "warning", solidHeader = TRUE, width = 12,
                    div(plotlyOutput("yearlyDelaysGraph", height= 650))
                )
            
              )
      ),
      
      tabItem(tabName = "hourlytotal",
              fluidRow(
                box(width = 12,
                       useShinyjs(),
                       checkboxInput("timeChoice", "Pick a specific Date", FALSE),
                       div(dateInput("dateHourly", label = h3("Specific Date"), value = "2017-01-01"))
                       ),
                
                
                tabBox(
                  width = 12,
                  id = "tabset2", height = "1000px",
                  tabPanel("O'Hare and Midway", div(plotlyOutput("hourlyGraph", height = 900))),
                  tabPanel("O'Hare", div(plotlyOutput("hourlyGraphORD", height = 900))),
                  tabPanel("Midway", div(plotlyOutput("hourlyGraphMID", height = 900)))
                ),
                box(status = "primary", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("totalselectedDataTable", height = 800)
                ),
                tabBox(
                  title = "Yearly",
                  width = 6,
                  id = "tabset1",
                  tabPanel("Arrivals", div(plotlyOutput("hourlyYearGraphArr", height = 800))),
                  tabPanel("Departures", div(plotlyOutput("hourlyYearGraphDep", height = 800)))
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
                    id = "monthTopCarriers", width = "100%", height = "900px",
                    tabPanel("Common Scale", div(plotlyOutput("popularGraph", height = 800) %>% withSpinner(color="#0dc5c1"))),
                    tabPanel("Midway", div(plotlyOutput("popularGraphMIDWAY", height = 800) %>% withSpinner(color="#0dc5c1"))),
                    tabPanel("O'Hare", div(plotlyOutput("popularGraphOHARE", height = 800) %>% withSpinner(color="#0dc5c1"))),
                    tabPanel("Table", div(DT::dataTableOutput("topCarriersTable") %>% withSpinner(color="#0dc5c1")))
                  )
                ),
                box(title = "Year View - Top Airlines Total Departures/Arrivals", solidHeader = TRUE, status = "primary", width = 6,
                    tabBox(
                      title = "January - December 2017",
                      id = "yearTopCarriers", height = "900px", width = "100%",
                      tabPanel("Common Scale", div(plotlyOutput("allMonthsPopularGraph", height = 800)  %>% withSpinner(color="#0dc5c1"))),
                      tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")  %>% withSpinner(color="#0dc5c1")))
                    )
                )
              ),
              
              # User selects CARRIER from list of available airlines
              fluidRow(
                box(title = "Departures/Arrivals for Selected Airline", solidHeader = TRUE, status = "primary", width = 12,
                    selectInput("airline-dropdown", "Airline:", choices = as.character(allPopularCarriers$CARRIER)),
                    dateInput("date-selectCarrier", label = "Select Date", format = "yyyy-mm-dd", value = "2017-01-01"),
                    tabBox(
                      title = textOutput('carrierText', inline = TRUE),
                      id = "twentyFourTopCarriers", height = "500px", width = "100%",
                      tabPanel("Graph", div(plotlyOutput("specificCarrier24Plot", height = 400)  %>% withSpinner(color="#0dc5c1")))#,
                      #tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")))
                    ),
                    tabBox(
                      title = textOutput('carrierText2', inline = TRUE),
                      id = "yearChosenTopCarrier", height = "500px", width = "100%",
                      tabPanel("Graph", div(plotlyOutput("specificCarrierYearPlot", height = 400)  %>% withSpinner(color="#0dc5c1")))#,
                      #tabPanel("Table", div(DT::dataTableOutput("allMonthsTopCarriersTable")))
                    )
                )
              )
      ),
      
      tabItem(tabName = "weeklyTotal",
              fluidRow(
                box(title = "Month View - Total Departures/Arrivals per Weekday", solidHeader = TRUE, status = "primary", width = 12,
                  tabBox(
                    title = textOutput('mWeekdayText', inline = TRUE),
                    id = "weekdayTab", height = "700px", width = "100%",
                    tabPanel("Common Scale", plotlyOutput("weekdayGraph", height = 650) %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Midway", plotlyOutput("weekdayGraphMIDWAY", height = 650) %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Ohare", plotlyOutput("weekdayGraphOHARE", height = 650) %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Table", DT::dataTableOutput("weekdayTable"))
                  )
                )
              ),
              # User selects WEEKDAY (i.e. Sunday, Monday, etc.)
              fluidRow(
                box(title = "Departures/Arrivals per Selected Weekday", solidHeader = TRUE, status = "primary", width = 12,
                    height = "700px",
                    selectInput("weekday-select", label = "Day of Week", list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7), selected = 7),
                    tabBox(
                      title = textOutput('weekdayText', inline = TRUE),
                       width = "100%",
                      tabPanel("Graph", plotlyOutput("specificWeekdayYearPlot", height = 650) %>% withSpinner(color="#0dc5c1"))
                    ),
                    tabBox(
                      title = textOutput('weekdayText2', inline = TRUE),
                      height = "700px", width = "100%",
                      tabPanel("Graph", 
                               radioGroupButtons(
                                 inputId = "delayButtons2", label = "Types of Delay :",
                                 choices = c("Carrier", "Weather", "National Air System", "Security", "Late Aircraft"),
                                 justified = TRUE, status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                               ),
                               plotlyOutput("specificWeekdayDelayPlot", height = 650) %>% withSpinner(color="#0dc5c1"))
                    )
                    
                )
              )
      ),
      tabItem(tabName = "int-days",
              box(selectInput("interestingDate", "Interesting days:", choices = as.character(interesting$Event)),
                  tabBox(
                    title = "Hourly Delays",
                    width = 12,
                    id = "tabset3", height = "1000px",
                    tabPanel("O'Hare and Midway",
                             radioGroupButtons(
                               inputId = "delayButtons3", label = "Types of Delay :",
                               choices = c("Carrier", "Weather", "National Air System", "Security", "Late Aircraft"),
                               justified = TRUE, status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                             ),
                             
                             box(status = "primary", solidHeader = TRUE, width = 12,
                                 div(plotlyOutput("delayGraph2", height = 900))
                             ),
                             box("O'Hare and Midway", div(plotlyOutput("hourlyGraphInt", height = 900)))
                  ))
                  ,width = 12
              )),
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
              h4("shinyWidgets"),
              h4("shinycssloaders")
      )
    )
    
  )
)

