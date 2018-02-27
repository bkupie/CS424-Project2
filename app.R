# Project 2 for CS 424 Spring 2018 UIC 
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
library(dplyr)
library(plotly)
library(shinyWidgets)


# process dataset here
decFlights <- read.table(file = "decData.csv", sep = ",", header = TRUE)

#create new column that converts minutes to hour:minute
decFlights$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", decFlights$DEP_TIME), format='%H%M')
decFlights$DEP_TIMEaggregated <- cut(decFlights$DEP_TIMEaggregated, breaks = "hour")
decFlights$DEP_TIMEaggregated <- substr(decFlights$DEP_TIMEaggregated, 12, 16)

decFlights$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", decFlights$ARR_TIME), format='%H%M')
decFlights$ARR_TIMEaggregated <- cut(decFlights$ARR_TIMEaggregated, breaks = "hour")
decFlights$ARR_TIMEaggregated <- substr(decFlights$ARR_TIMEaggregated, 12, 16)

#count based on hour
hourlyDepartures <- aggregate(cbind(count = CARRIER) ~ DEP_TIMEaggregated, 
                              data = decFlights, 
                              FUN = function(x){NROW(x)})

hourlyArrivals <- aggregate(cbind(count = CARRIER) ~ ARR_TIMEaggregated, 
                            data = decFlights, 
                            FUN = function(x){NROW(x)})

#add nicer names to columns
names(hourlyDepartures) <- c("Hour", "Count")
names(hourlyArrivals) <- c("Hour", "Count")

#merge into total decFlights for both departure and arrival
totaldecFlights <- merge(hourlyDepartures,hourlyArrivals,by="Hour")

#give nicer column names
names(totaldecFlights) <- c("Hour", "Departures", "Arrivals")

#add int boolean for if delay exists or not
decFlights$delayTrue<-ifelse(decFlights$ARR_DELAY_NEW>0 | decFlights$DEP_DELAY_NEW > 0,1,0)

#count arrival delays per hour
hourlyDelayCount <- aggregate(cbind(count = delayTrue) ~ ARR_TIMEaggregated,
                                 data = decFlights,
                                 FUN = sum)

#give niver column names
names(hourlyDelayCount) <- c("Hour", "Count")

#create new table that will also hold percentage
totaldecFlightsPercentage <- merge(totaldecFlights,hourlyDelayCount,by="Hour")
totaldecFlightsPercentage$Percentage <- (totaldecFlightsPercentage$Count / (totaldecFlightsPercentage$Departures + totaldecFlightsPercentage$Arrivals)) * 100

#drop arrivals and departures from table
totaldecFlightsPercentage <- subset(totaldecFlightsPercentage, select = -c(2,3) )

#round percentage
totaldecFlightsPercentage$Percentage <-round(totaldecFlightsPercentage$Percentage, 0)

#give nicer column names
#names(totaldecFlightsPercentage) <- c("Hour", "Total Delays", "% of decFlights")


#-----------------------------------------------------------
#bart starts here
#count locations based on amount of origin
totalOrigin <- aggregate(cbind(count = ORIGIN_CITY_NAME) ~ ORIGIN_CITY_NAME, 
                             data = decFlights, 
                             FUN = function(x){NROW(x)})

#count locations based on amount of destination
totalDest <- aggregate(cbind(count = DEST_CITY_NAME) ~ DEST_CITY_NAME, 
                             data = decFlights, 
                             FUN = function(x){NROW(x)})

#quick rename before we can join them
names(totalOrigin) <- c("City Name", "Count Origin")
names(totalDest) <- c("City Name", "Count Destination")

#now we combine the two totals togheter
totalDepartures <- merge(totalDest,totalOrigin,by="City Name")
totalDepartures$"Total Count" <- totalDepartures$"Count Origin" +totalDepartures$"Count Destination"
#last step is to sort by total count 
totalDepartures <- totalDepartures[order(-totalDepartures$"Total Count"),]

#-----------------------------------------------------------

# start up the gui 
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bart", tabName = "bart", icon = icon("dashboard")),
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
              h2("Bart tab content"),
              box(title = ":-)", solidHeader = TRUE, status = "primary", width = 10,
                  dataTableOutput("bartTable1")
              ),
              box(title = "Bart chart!!!", solidHeader = TRUE, width = 10,
                  div(sliderInput("topChoices", "Top decFlights number", 
                                  min = 1, max = 50, value = 15, width = 250)),
                  div(plotlyOutput("bartChart1"))
              )
      ),
      
      
      tabItem(tabName = "delays",
              fluidRow(
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("delayGraph"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                    DT::dataTableOutput("totaldecFlightsPercentageTable")
                )
                
              )
      ),
      
      tabItem(tabName = "hourlytotal",
              fluidRow(
                box(status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                    div(plotlyOutput("hourlyGraph"))
                ),
                box(status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                    DT::dataTableOutput("totaldecFlightsTable")
                )
                
              )
      ),
      
      tabItem(tabName = "vijay",
              h2("Vijay tab content")
      ),
      tabItem(tabName = "info",
              h1("Project 2 for CS 424 Spring 2018 UIC"),
              h2("Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae"),
              h2("Libraries used:")
      )
    )
    
  )
)


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
  
  #isabel outputs
  output$totaldecFlightsTable <- renderDataTable(totaldecFlights, extensions = 'Scroller', rownames = FALSE, options = list(
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE,
    bFilter=0
  ))
  
  output$totaldecFlightsPercentageTable <- renderDataTable(totaldecFlightsPercentage, extensions = 'Scroller', rownames = FALSE, options = list(
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE,
    bFilter=0
  ))
  
  output$hourlyGraph <- renderPlotly({
    plot_ly(hourlyDepartures, x = ~hourlyDepartures$Hour, y = ~hourlyDepartures$Count, type = 'bar', name = 'Departures', hoverinfo = 'text',
            text = ~paste('</br>', hourlyDepartures$Count, ' Departures </br>'), marker = list(color = 'rgb(49,130,189)')) %>%
      add_trace(x = ~hourlyArrivals$Hour, y = ~hourlyArrivals$Count, name = 'Arrivals', hoverinfo = 'text',
                text = ~paste('</br>', hourlyArrivals$Count, ' Arrivals </br>'),
                
                marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = list(title = "Time Period", tickangle = -45),
             yaxis = list(title = "# of decFlights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  output$delayGraph <- renderPlotly({
    plot_ly(data = totaldecFlightsPercentage, x = ~totaldecFlightsPercentage$Hour, y = ~totaldecFlightsPercentage$Count, type = "bar", showlegend=TRUE, hoverinfo = 'text',
            text = ~paste('</br>', Count, ' Delays </br>',
                          Percentage, '% of decFlights</br>'),
            marker=list(color=~totaldecFlightsPercentage$Percentage, showscale=TRUE)) %>% layout(xaxis = list(title = "Time Period", tickangle = -45),yaxis = list(title = "# of decFlights"),
                                                                                      margin = list(b = 100),
                                                                                      barmode = 'group')
                                                                                      
  })
  
  
                                                                          
  
  #bart outputs 
  #render the table for departure/arrival counters
  output$bartTable1 <- DT::renderDataTable(
    DT::datatable({ 
      #show only the top 15 
      head(totalDepartures,15)

  
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE)
  )
  )
  
  output$bartChart1 <- renderPlotly({
    df <- totalDepartures
    # get only the top 15 locations
    df <- df  %>% top_n(15)
    plot_ly(df, x = ~df$"City Name", y = ~df$"Count Destination", type = 'bar',name = 'Count Destination', text = paste("Total for city:" ,  (df$"Total Count"))) %>%
      add_trace(y =  ~df$"Count Origin", name = 'Count Origin') %>%
      layout(xaxis = list(title = "City Name", tickangle = -45),
             yaxis = list(title = "# of decFlights"),
             barmode = 'stack',
             margin = list(b = 100)
             )
  })
  

}  
  
#start the actual application 
shinyApp(ui = ui, server = server)
