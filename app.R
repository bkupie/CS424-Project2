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
# flights dataset with carrier names in it (i.e. Spirit Airlines, etc.)
cleanedFlights <- read.csv(file = "cleaned-flights.csv")

# ordering flights by most common airlines
# this takes top 20 of dataframe # top20airlines <- cleanedFlights %>% top_n(20)
# based closely on following tutorial: https://rstudio-pubs-static.s3.amazonaws.com/52879_eaa8e7a9919b4bb6a2cf6e2bda587cb1.html
cleanedFlights$CARRIER <- as.character(cleanedFlights$CARRIER)
popularCarriers <- data.frame(summarize(group_by(cleanedFlights, CARRIER), sum(FR)))
popularCarriers <- arrange(popularCarriers, -popularCarriers$sum.FR)
popularCarriers$MIDWAY_DEPARTURES <- NA
popularCarriers$OHARE_DEPARTURES <- NA
popularCarriers$MIDWAY_ARRIVALS <- NA
popularCarriers$OHARE_ARRIVALS <- NA

# Midway airport ID = 13232; O'Hare airport ID = 13930
for(i in 1:length(popularCarriers$CARRIER)) {
  top1_MID <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
  top1_MID = top1_MID %>% filter(ORIGIN_AIRPORT == "Chicago Midway International")
  top1_MID = data.frame(summarize(group_by(top1_MID, CARRIER), sum(FR)))
  if (is.na(top1_MID$sum.FR.) || length(top1_MID$sum.FR.) == 0)
  {
    popularCarriers$MIDWAY_DEPARTURES[i] <- 0
  } else {
    popularCarriers$MIDWAY_DEPARTURES[i] <- top1_MID$sum.FR.
  }

  top1_OHARE <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
  top1_OHARE = top1_OHARE %>% filter(ORIGIN_AIRPORT == "Chicago O'Hare International")
  top1_OHARE = data.frame(summarize(group_by(top1_OHARE, CARRIER), sum(FR)))
  if (is.na(top1_OHARE$sum.FR.) || length(top1_OHARE$sum.FR.) == 0)
  {
    popularCarriers$OHARE_DEPARTURES[i] <- 0
  } else {
    popularCarriers$OHARE_DEPARTURES[i] <- top1_OHARE$sum.FR.
  }
}

for(i in 1:length(popularCarriers$CARRIER)) {
  top2_MID <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
  top2_MID = top2_MID %>% filter(DEST_AIRPORT == "Chicago Midway International")
  top2_MID = data.frame(summarize(group_by(top2_MID, CARRIER), sum(FR)))
  if (is.na(top2_MID$sum.FR.) || length(top2_MID$sum.FR.) == 0)
  {
    popularCarriers$MIDWAY_ARRIVALS[i] <- 0
  } else {
    popularCarriers$MIDWAY_ARRIVALS[i] <- top2_MID$sum.FR.
  }
  
  top2_OHARE <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
  top2_OHARE = top2_OHARE %>% filter(ORIGIN_AIRPORT == "Chicago O'Hare International")
  top2_OHARE = data.frame(summarize(group_by(top2_OHARE, CARRIER), sum(FR)))
  if (is.na(top2_OHARE$sum.FR.) || length(top2_OHARE$sum.FR.) == 0)
  {
    popularCarriers$OHARE_ARRIVALS[i] <- 0
  } else {
    popularCarriers$OHARE_ARRIVALS[i] <- top2_OHARE$sum.FR.
  }
}

#test.csv is for isabel atm SWTICH TO CORRECT FILE IF YOU NEED IT
#correct file = "ontime_flights.cleaned.csv"
flights <- read.table(file = "test.csv", sep = ",", header = TRUE)

# this fixes your data
selectedData <- read.csv(file = "cleaned-flights.csv")

#create new column that converts minutes to hour:minute
selectedData$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$DEP_TIME), format='%H%M')
selectedData$DEP_TIMEaggregated <- cut(selectedData$DEP_TIMEaggregated, breaks = "hour")
selectedData$DEP_TIMEaggregated <- substr(selectedData$DEP_TIMEaggregated, 12, 16)

selectedData$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
selectedData$ARR_TIMEaggregated <- cut(selectedData$ARR_TIMEaggregated, breaks = "hour")
selectedData$ARR_TIMEaggregated <- substr(selectedData$ARR_TIMEaggregated, 12, 16)

#count based on hour
hourlyDepartures <- aggregate(cbind(count = CARRIER) ~ DEP_TIMEaggregated,
                              data = selectedData,
                              FUN = function(x){NROW(x)})

hourlyArrivals <- aggregate(cbind(count = CARRIER) ~ ARR_TIMEaggregated,
                            data = selectedData,
                            FUN = function(x){NROW(x)})

#add nicer names to columns
names(hourlyDepartures) <- c("Hour", "Count")
names(hourlyArrivals) <- c("Hour", "Count")

#merge into total selectedData for both departure and arrival
totalselectedData <- merge(hourlyDepartures,hourlyArrivals,by="Hour")

#give nicer column names
names(totalselectedData) <- c("Hour", "Departures", "Arrivals")

#add int boolean for if delay exists or not
selectedData$delayTrue<-ifelse(selectedData$ARR_DELAY_NEW>0 | selectedData$DEP_DELAY_NEW > 0,1,0)

carrierDelay <- subset(selectedData, CARRIER_DELAY > 0)
securityDelay <- subset(selectedData, SECURITY_DELAY > 0)


#count arrival delays per hour
hourlyDelayCount <- aggregate(cbind(count = delayTrue) ~ ARR_TIMEaggregated,
                                 data = selectedData,
                                 FUN = sum)

#give niver column names
names(hourlyDelayCount) <- c("Hour", "Count")

#create new table that will also hold percentage
totalselectedDataPercentage <- merge(totalselectedData,hourlyDelayCount,by="Hour")
totalselectedDataPercentage$Percentage <- (totalselectedDataPercentage$Count / (totalselectedDataPercentage$Departures + totalselectedDataPercentage$Arrivals)) * 100

#drop arrivals and departures from table
totalselectedDataPercentage <- subset(totalselectedDataPercentage, select = -c(2,3) )

#round percentage
totalselectedDataPercentage$Percentage <-round(totalselectedDataPercentage$Percentage, 0)

#give nicer column names
#names(totalselectedDataPercentage) <- c("Hour", "Total Delays", "% of selectedData")


#-----------------------------------------------------------
#bart starts here
#count locations based on amount of origin
totalOrigin <- aggregate(cbind(count = ORIGIN_CITY_NAME) ~ ORIGIN_CITY_NAME,
                             data = selectedData,
                             FUN = function(x){NROW(x)})

#count locations based on amount of destination
totalDest <- aggregate(cbind(count = DEST_CITY_NAME) ~ DEST_CITY_NAME,
                             data = selectedData,
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
                  div(sliderInput("topChoices", "Top selectedData number",
                                  min = 1, max = 50, value = 15, width = 250)),
                  div(plotlyOutput("bartChart1"))
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


server <- function(input, output) {

  v <- reactiveValues(data = NULL)
  #cleanedData <- data.table(NULL)

  observeEvent(input$delayButtons, {
    if(input$delayButtons == 'Carrier'){
      v$data <- carrierDelay
    }
    else{
      v$data <- securityDelay
    }

    #count arrival delays per hour
    hourlyDelayCount <- aggregate(cbind(count = delayTrue) ~ ARR_TIMEaggregated,
                                  data = v$data,
                                  FUN = sum)

    #give niver column names
    names(hourlyDelayCount) <- c("Hour", "Count")

    # #create new table that will also hold percentage
    # cleanedData <- merge(totalselectedData,hourlyDelayCount,by="Hour")
    # cleanedData$Percentage <- (cleanedData$Count / (cleanedData$Departures + cleanedData$Arrivals)) * 100
    #
    # #drop arrivals and departures from table
    # cleanedData <- subset(cleanedData, select = -c(2,3) )
    #
    # #round percentage
    # cleanedData$Percentage <-round(cleanedData$Percentage, 0)

  })

  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })

  # increase the default font size
  theme_set(theme_dark(base_size = 18))

  #isabel outputs
  output$totalselectedDataTable <- renderDataTable(totalselectedData, extensions = 'Scroller', rownames = FALSE, options = list(
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE,
    bFilter=0
  ))

  output$totalselectedDataPercentageTable <- renderDataTable(totalselectedDataPercentage, extensions = 'Scroller', rownames = FALSE, options = list(
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
             yaxis = list(title = "# of selectedData"),
             margin = list(b = 100),
             barmode = 'group')
  })

  output$delayGraph <- renderPlotly({
    #if (v$data == 'Carrier') return()
    #cleanedData
    plot_ly(data =  totalselectedDataPercentage, x = ~totalselectedDataPercentage$Hour, y = ~totalselectedDataPercentage$Count, type = "bar", showlegend=TRUE, hoverinfo = 'text',
            text = ~paste('</br>', Count, ' Delays </br>',
                          Percentage, '% of selectedData</br>'),
            marker=list(color=~totalselectedDataPercentage$Percentage, showscale=TRUE)) %>% layout(xaxis = list(title = "Time Period", tickangle = -45),yaxis = list(title = "# of selectedData"),
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
             yaxis = list(title = "# of selectedData"),
             barmode = 'stack',
             margin = list(b = 100)
             )
  })
  
  # bar chart of top carriers Departure and Arrival times
  
  
  # table for top carriers Departure and Arrival times
  output$topCarriers <- DT::renderDataTable(
    DT::datatable({
      popularCarriers
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    )
    )
  )


}

#start the actual application
shinyApp(ui = ui, server = server)
