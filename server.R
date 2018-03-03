

server <- function(input, output) {

# process dataset here


#first read in the lookup tables that will be needed 
airlineLookup <- read.table(file = "data/airline.csv", sep = ",", header = TRUE)
carrierLookup <- read.table(file = "data/carrier.csv", sep = ",", header = TRUE)
#rename columns to properly line up 
names(airlineLookup) <- c("AIRLINE_ID", "AIRLINE_DESCRIPTION")
names(carrierLookup) <- c("CARRIER", "CARRIER_NAME")
#writing a function to parse a month file into a correct data frame  
parseMonth <- function(fileName)
{
  #first we read in the file in the regular way 
  parsedMonth <- read.table(file = fileName, sep = ",", header = TRUE)
  parsedMonth$CARRIER <- as.character(parsedMonth$CARRIER)
  #now combine with all the other tables we have put in
  parsedMonth <- merge(parsedMonth,carrierLookup,by="CARRIER")
  parsedMonth <- merge(parsedMonth,airlineLookup,by="AIRLINE_ID")
  
  
  #write code for cleaning up NA fields 
  
  #return cleaned data 
  return(parsedMonth)
  
}

# flights dataset with carrier names in it (i.e. Spirit Airlines, etc.)
cleanedFlights <- read.csv(file = "data/cleaned-flights.csv")

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
flights <- read.table(file = "data/test.csv", sep = ",", header = TRUE)

# this fixes your data
selectedData <- read.csv(file = "data/cleaned-flights.csv")

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

airportTotals <- function(airport_name)
{

  
  #count locations based on amount of origin
  totalOrigin <- selectedData %>% filter(DEST_AIRPORT == airport_name)
  totalOrigin <- aggregate(cbind(count = ORIGIN_AIRPORT) ~ ORIGIN_AIRPORT,
                           data = totalOrigin,
                           FUN = function(x){NROW(x)})
  
  #count locations based on amount of destination
  totalDest <- selectedData %>% filter(ORIGIN_AIRPORT == airport_name)
  totalDest <- aggregate(cbind(count = DEST_AIRPORT) ~ DEST_AIRPORT,
                         data = totalDest,
                         FUN = function(x){NROW(x)})
  
  #quick rename before we can join them
  names(totalOrigin) <- c("Airport Name", "Count Origin")
  names(totalDest) <- c("Airport Name", "Count Destination")
  
  #now we combine the two totals togheter
  totalDepartures <- merge(totalDest,totalOrigin,by="Airport Name")
  totalDepartures$"Total Count" <- totalDepartures$"Count Origin" +totalDepartures$"Count Destination"
  #last step is to sort by total count
  totalDepartures <- totalDepartures[order(-totalDepartures$"Total Count"),]
  
  
  return(totalDepartures)
  
    
}
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
  
  # set up the margins for graphs
  graphMargins <- list(
    l = 50,
    r = 150,
    b = 200,
    t = 100,
    pad = 4
  )
  
  #render the table for departure/arrival counters
  output$bartTable1 <- DT::renderDataTable(
    DT::datatable({
      #show only the top 15
      df <- airportTotals("Chicago O'Hare International")
      head(df,15)
    },
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE)
    )
  )
  
  output$bartTable2 <- DT::renderDataTable(
    DT::datatable({
      #show only the top 15
      df <- airportTotals("Chicago Midway International")
      head(df,15)
    },
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE)
    )
  )
  
  output$bartChart1 <- renderPlotly({
    df <- airportTotals("Chicago O'Hare International")
    # get only the top 15 locations
    df <- df  %>% top_n(15)
    
    plot_ly(df, x = ~df$"Airport Name", y = ~df$"Count Destination", type = 'bar',name = 'Count Destination', text = paste("Total for airport:" ,  (df$"Total Count"))) %>%
      add_trace(y =  ~df$"Count Origin", name = 'Count Origin') %>%
      layout(xaxis = list(categoryorder = "array",categoryarray = df$"Airport Name", title = "Airport Name", tickangle = -45),
             yaxis = list(title = "Airport Connections"),
             barmode = 'stack',
             margin = graphMargins
      )
  })
  
  output$bartChart2 <- renderPlotly({
    df <- airportTotals("Chicago Midway International")
    # get only the top 15 locations
    df <- df  %>% top_n(15)
    
    plot_ly(df, x = ~df$"Airport Name", y = ~df$"Count Destination", type = 'bar',name = 'Count Destination', text = paste("Total for airport:" ,  (df$"Total Count"))) %>%
      add_trace(y =  ~df$"Count Origin", name = 'Count Origin') %>%
      layout(xaxis = list(categoryorder = "array",categoryarray = df$"Airport Name", title = "Airport Name", tickangle = -45),
             yaxis = list(title = "Airport Connections"),
             barmode = 'stack',
             margin = graphMargins
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

