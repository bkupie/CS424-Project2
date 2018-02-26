# Project 1 for CS 424 Spring 2018 UIC 
# Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae
# for now this is just a template we may end up using, but don't have to

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
library(plyr)
library(plotly)


# process dataset here
flights <- read.table(file = "ontime_flights.cleaned.csv", sep = ",", header = TRUE)

#create new column that converts minutes to hour:minute
flights$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", flights$DEP_TIME), format='%H%M')
flights$DEP_TIMEaggregated <- cut(flights$DEP_TIMEaggregated, breaks = "hour")
flights$DEP_TIMEaggregated <- substr(flights$DEP_TIMEaggregated, 12, 16)

flights$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", flights$ARR_TIME), format='%H%M')
flights$ARR_TIMEaggregated <- cut(flights$ARR_TIMEaggregated, breaks = "hour")
flights$ARR_TIMEaggregated <- substr(flights$ARR_TIMEaggregated, 12, 16)

#count based on hour
hourlyDepartures <- aggregate(cbind(count = CARRIER) ~ DEP_TIMEaggregated, 
                              data = flights, 
                              FUN = function(x){NROW(x)})

hourlyArrivals <- aggregate(cbind(count = CARRIER) ~ ARR_TIMEaggregated, 
                            data = flights, 
                            FUN = function(x){NROW(x)})

#add nicer names to columns
names(hourlyDepartures) <- c("Departure Hour", "Count")
names(hourlyArrivals) <- c("Arrival Hour", "Count")

#count locations based on amount of origin
totalOrigin <- aggregate(cbind(count = ORIGIN_AIRPORT_ID) ~ ORIGIN_AIRPORT_ID, 
                             data = flights, 
                             FUN = function(x){NROW(x)})

#count locations based on amount of destination
totalDest <- aggregate(cbind(count = DEST_AIRPORT_ID) ~ DEST_AIRPORT_ID, 
                             data = flights, 
                             FUN = function(x){NROW(x)})

#quick rename before we can join them
names(totalOrigin) <- c("ID", "Count Origin")
names(totalDest) <- c("ID", "Count Destination")

#now we combine the two totals togheter
totalDepartures <- merge(totalDest,totalOrigin,by="ID")
totalDepartures$"Total Count" <- totalDepartures$"Count Origin" +totalDepartures$"Count Destination"



# start up the gui 
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bart", tabName = "bart", icon = icon("dashboard")),
      menuItem("Isabel", icon = icon("th"), tabName = "isabel"),
      menuItem("Vijay", tabName = "vijay", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "bart",
              h2("Bart tab content"),
              box(title = ":-)", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable1")
              )
      ),
      
      
      tabItem(tabName = "isabel",
              fluidRow(
                tabBox(width = 4,
                       title = "Flights by Hour",
                       id = "tabset1",
                       tabPanel("Arrivals", DT::dataTableOutput("hourlyArrivalTable", height = "100%")),
                       tabPanel("Departures", DT::dataTableOutput("hourlyDepartureTable", height = "100%"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 8, height = NULL,
                    div(plotlyOutput("hourlyGraph"))
                )
                
              )
      ),
      
      tabItem(tabName = "vijay",
              h2("Vijay tab content")
      )
    )
    
  )
)


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
  
  output$hourlyDepartureTable = DT::renderDataTable({
    hourlyDepartures
  }, rownames= FALSE, options=list(paging = FALSE, bFilter=0, bInfo=0, bLengthChange = FALSE)
  )
  
  output$hourlyArrivalTable = DT::renderDataTable({
    hourlyArrivals
  }, rownames= FALSE, options=list(paging = FALSE, bFilter=0, bInfo=0, bLengthChange = FALSE)
  )
  
  output$hourlyGraph <- renderPlotly({
    plot_ly(hourlyDepartures, x = ~hourlyDepartures$`Departure Hour`, y = ~hourlyDepartures$Count, type = 'bar', name = 'Departures', marker = list(color = 'rgb(49,130,189)')) %>%
      add_trace(x = ~hourlyArrivals$`Arrival Hour`, y = ~hourlyArrivals$Count, name = 'Arrivals', marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = list(title = "Time Period", tickangle = -45),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  #bart outputs 
  #render the table for 
  output$bartTable1 <- DT::renderDataTable(
    DT::datatable({ 
      #show only the top 15 
      top15 = totalDepartures[sample(nrow(totalDepartures), 15), ]  
  
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE, order = list(list(3, 'dec')))
  )
  )
}  
  
#start the actual application 
shinyApp(ui = ui, server = server)
