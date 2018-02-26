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
#test.csv is for isabel atm SWTICH TO CORRECT FILE IF YOU NEED IT
#correct file = "ontime_flights.cleaned.csv"
flights <- read.table(file = "test.csv", sep = ",", header = TRUE)

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
names(hourlyDepartures) <- c("Hour", "Count")
names(hourlyArrivals) <- c("Hour", "Count")

#merge into total flights for both departure and arrival
totalFlights <- merge(hourlyDepartures,hourlyArrivals,by="Hour")

#give nicer column names
names(totalFlights) <- c("Hour", "Departures", "Arrivals")

#add int boolean for if delay exists or not
flights$delayTrue<-ifelse(flights$ARR_DELAY_NEW>0 | flights$DEP_DELAY_NEW > 0,1,0)

#count arrival delays per hour
hourlyDelayCount <- aggregate(cbind(count = delayTrue) ~ ARR_TIMEaggregated,
                                 data = flights,
                                 FUN = sum)

#give niver column names
names(hourlyDelayCount) <- c("Hour", "Count")

#create new table that will also hold percentage
totalFlightsPercentage <- merge(totalFlights,hourlyDelayCount,by="Hour")
totalFlightsPercentage$Percentage <- (totalFlightsPercentage$Count / (totalFlightsPercentage$Departures + totalFlightsPercentage$Arrivals)) * 100

#drop arrivals and departures from table
totalFlightsPercentage <- subset(totalFlightsPercentage, select = -c(2,3) )

#round percentage
totalFlightsPercentage$Percentage <-round(totalFlightsPercentage$Percentage, 0)

#give niver column names
#names(totalFlightsPercentage) <- c("Hour", "Total Delays", "% of Flights")

#bart starts here
#count locations based on amount of origin
totalOrigin <- aggregate(cbind(count = ORIGIN_CITY_NAME) ~ ORIGIN_CITY_NAME, 
                             data = flights, 
                             FUN = function(x){NROW(x)})

#count locations based on amount of destination
totalDest <- aggregate(cbind(count = DEST_CITY_NAME) ~ DEST_CITY_NAME, 
                             data = flights, 
                             FUN = function(x){NROW(x)})

#quick rename before we can join them
names(totalOrigin) <- c("City Name", "Count Origin")
names(totalDest) <- c("City Name", "Count Destination")

#now we combine the two totals togheter
totalDepartures <- merge(totalDest,totalOrigin,by="City Name")
totalDepartures$"Total Count" <- totalDepartures$"Count Origin" +totalDepartures$"Count Destination"
#last step is to sort by total count 
totalDepartures <- totalDepartures[order(-totalDepartures$"Total Count"),]



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
                box(status = "warning", solidHeader = TRUE, width = 4, height = NULL,
                    #DT::dataTableOutput("totalFlightsTable")
                    DT::dataTableOutput("totalFlightsPercentageTable")
                ),
                box(status = "primary", solidHeader = TRUE, width = 8, height = NULL,
                    #div(plotlyOutput("hourlyGraph"))
                    div(plotlyOutput("delayGraph"))
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
  
  #isabel outputs
  output$totalFlightsTable = DT::renderDataTable({
    totalFlights
  }, rownames= FALSE, options=list(paging = FALSE, bFilter=0, bInfo=0, bLengthChange = FALSE)
  )
  
  output$totalFlightsPercentageTable = DT::renderDataTable({
    totalFlightsPercentage
  }, rownames= FALSE, options=list(paging = FALSE, bFilter=0, bInfo=0, bLengthChange = FALSE)
  )
  
  output$hourlyGraph <- renderPlotly({
    plot_ly(hourlyDepartures, x = ~hourlyDepartures$Hour, y = ~hourlyDepartures$Count, type = 'bar', name = 'Departures', marker = list(color = 'rgb(49,130,189)')) %>%
      add_trace(x = ~hourlyArrivals$Hour, y = ~hourlyArrivals$Count, name = 'Arrivals', marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = list(title = "Time Period", tickangle = -45),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  output$delayGraph <- renderPlotly({
    plot_ly(data = totalFlightsPercentage, x = ~totalFlightsPercentage$Hour, y = ~totalFlightsPercentage$Count, type = "bar", showlegend=FALSE, hoverinfo = 'text',
            text = ~paste('</br>', Count, ' Delays </br>',
                          Percentage, '% of Flights</br>'),
            marker=list(color=~totalFlightsPercentage$Percentage, showscale=FALSE)) %>% layout(xaxis = list(title = "Time Period", tickangle = -45),yaxis = list(title = "# of Flights"),
                                                                                      margin = list(b = 100),
                                                                                      barmode = 'group')
                                                                                      
  })
  
  
                                                                          
  
  #bart outputs 
  #render the table for 
  output$bartTable1 <- DT::renderDataTable(
    DT::datatable({ 
      #show only the top 15 
      head(totalDepartures,15)
      #top15 = totalDepartures[sample(nrow(totalDepartures), 15), ]  
  
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE)
  )
  )
}  
  
#start the actual application 
shinyApp(ui = ui, server = server)
