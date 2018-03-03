#libraries to include
library(shiny)
library(shinydashboard)
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

# start up the gui
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top 15 Airports", tabName = "bart", icon = icon("dashboard")),
      menuItem("Delays", icon = icon("hourglass", lib = "font-awesome"), tabName = "delays"),
      menuItem("Hourly Total", icon = icon("hourglass", lib = "font-awesome"), tabName = "hourlytotal"),
      menuItem("Vijay", tabName = "vijay", icon = icon("dashboard")),
      
      #get month
      selectInput("select", label = h5("Month"),
                  choices = list("January" = 1, "February" = 2, "March" = 3,
                                 "April" = 4, "May" = 5, "June" = 6,
                                 "July" = 7, "August" = 8, "September" = 9,
                                 "October" = 10, "November" = 11, "December" = 12), selected = 1),
      #get day of week
      selectInput("select", label = h5("Day of Week"),
                  choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3,
                                 "Thursday" = 4, "Friday" = 5, "Saturday" = 6,
                                 "Sunday" = 7, "All" = 8), selected = 8),
      
      #get specific date !!! needs to be worked on
      dateInput("date", label = h5("Specific Date")),
      
      #info
      menuItem("Info", tabName = "info", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "bart",
              h2("Flights in/from Chicago O'Hare International/Midway"),
              fluidRow( box(title = "Flights in/from Chicago O'Hare International", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable1")
              ),
              box(title = "Flights in/from Chicago O'Hare International", solidHeader = TRUE, width = 10,
                  div(plotlyOutput("bartChart1"))
              )),
              box(title = "Flights in/from Chicago Midway International", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable2")
              ),
              box(title = "Flights in/from Chicago Midway International", solidHeader = TRUE, width = 10,
                  div(plotlyOutput("bartChart2"))
              )
              
      ),
      
      
      tabItem(tabName = "delays",
              fluidRow(
                radioGroupButtons(
                  inputId = "delayButtons", label = "Types of Delay :",
                  choices = c("Carrier", "Weather", " National Air System", "Security", "Late Aircraft"),
                  justified = TRUE, status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                ),
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("delayGraph"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                    DT::dataTableOutput("totalselectedDataPercentageTable")
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
                )
                
              )
      ),
      
      tabItem(tabName = "vijay",
              h4("Popular Carriers Info-Vis"),
              fluidRow(
                box(title = "Popular Carriers Arr-Dep Table", solidHeader = TRUE, status = "primary", width = 12,
                    DTOutput("topCarriers", width = "100%")
                )
              )
      ),
      tabItem(tabName = "info",
              h1("Project 2 for CS 424 Spring 2018 UIC"),
              h2("Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae"),
              h2("Libraries used:"),
              h2("shinydashboard"),
              h2("ggplot2"),
              h2("lubridate"),
              h2("DT"),
              h2("jpeg"),
              h2("grid"),
              h2("leaflet"),
              h2("reshape2"),
              h2("scales"),
              h2("dplyr"),
              h2("plotly"),
              h2("shinyWidgets")
      )
    )
    
  )
)

