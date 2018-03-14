server <- function(input, output) {

# FOR NOW WE ARE USING DECEMBER 2017 AS OUR INITIAL DATASET
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
      #TODO 
  #return cleaned data 
  return(parsedMonth)
  
}

# flights dataset with carrier names in it (i.e. Spirit Airlines, etc.)
cleanedFlights <- read.csv(file = "data/cleaned-flights.csv")

# ordering flights by day of the week (i.e. how many on all the mondays of the month and so forth)
# handy solution on how to group by days of the week found here: https://stackoverflow.com/questions/27828850/dplyr-does-not-group-data-by-date
# below we generate a column that has specific weekday of the flight date (i.e. 12/1/17 = Friday)
orderByWeekday <- cleanedFlights %>% 
  mutate(
    FL_DATE = parse_date_time(FL_DATE,"%m/%d/%y"),
    Weekday = wday(FL_DATE, label=TRUE, abbr=FALSE)
  )

midwayDeparturesByWeekday = orderByWeekday %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>% group_by(Weekday) %>% summarise(Total = n())
ohareDeparturesByWeekday = orderByWeekday %>% filter(ORIGIN_AIRPORT == "Chicago O'Hare International") %>% group_by(Weekday) %>% summarise(Total = n())

midwayArrivalsByWeekday = orderByWeekday %>% filter(DEST_AIRPORT == "Chicago O'Hare International") %>% group_by(Weekday) %>% summarise(Total = n())
ohareArrivalsByWeekday = orderByWeekday %>% filter(DEST_AIRPORT == "Chicago Midway International") %>% group_by(Weekday) %>% summarise(Total = n())

flightsByWeekday <- data.frame(
  Weekday = midwayDeparturesByWeekday$Weekday,
  MidwayDeparturesTotal = midwayDeparturesByWeekday$Total,
  OhareDeparturesTotal = ohareDeparturesByWeekday$Total,
  MidwayArrivalsTotal = midwayArrivalsByWeekday$Total,
  OhareArrivalsTotal = ohareArrivalsByWeekday$Total
)

# ordering flights by most common airlines
# based closely on following tutorial: https://rstudio-pubs-static.s3.amazonaws.com/52879_eaa8e7a9919b4bb6a2cf6e2bda587cb1.html
cleanedFlights$CARRIER <- as.character(cleanedFlights$CARRIER)
popularCarriers <- data.frame(summarize(group_by(cleanedFlights, CARRIER), sum(FR)))
popularCarriers <- arrange(popularCarriers, -popularCarriers$sum.FR.)
popularCarriers$MIDWAY_DEPARTURES <- NA
popularCarriers$OHARE_DEPARTURES <- NA
popularCarriers$MIDWAY_ARRIVALS <- NA
popularCarriers$OHARE_ARRIVALS <- NA

# Filter departures by only Midway and O'Hare Airports
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

# Filter arrivals by only Midway and O'Hare Airports
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

# Here I take all the months data and combine it into one list ==============================================
# Code referenced from here: https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
month_files = list.files(pattern = "\\.csv$")
month_data <- lapply(month_files, read.csv)

# To ensure list elements match the file names (i.e. january 2017 IL data = "IL_01Jan_2017")
names(month_data) <- gsub("\\.csv$", "", month_files)

# In example if you wish to access ALL of january's data from IL simply do the following
# month_data[[1]] OR month_data[["IL_01Jan_2017"]]
# Now say you want to access flight date from january's data from IL simply do the following
# month_data[[1]][1] OR month_data[["IL_01Jan_2017]]["FL_DATE"]

# Here I combine all 12 months of data into one LARGE dataframe
# TODO: I let the user pick which airport they want to look at.
ILData2017 <- rbind(month_data[[1]], month_data[[2]], month_data[[3]], month_data[[4]], month_data[[5]], month_data[[6]], month_data[[7]], month_data[[8]], month_data[[9]], month_data[[10]], month_data[[11]], month_data[[12]])
ILData2017$FR <- 1 # create frequency column to help rank popular carriers

# rank the top carriers
ILData2017$CARRIER <- as.character(ILData2017$CARRIER)
allPopularCarriers <- data.frame(summarize(group_by(ILData2017, CARRIER), sum(FR)))
allPopularCarriers <- arrange(allPopularCarriers, -allPopularCarriers$sum.FR.)

# rank the top airports
allPopularAirports <- data.frame(summarize(group_by(ILData2017, ORIGIN_AIRPORT_ID), sum(FR)))
allPopularAirports <- arrange(allPopularAirports, -allPopularAirports$sum.FR.)

# this monstrosity here takes the data across 12 months and filters it to Ohare and Midway
allPopularCarriers$MIDWAY_DEPARTURES <- NA
allPopularCarriers$OHARE_DEPARTURES <- NA
allPopularCarriers$MIDWAY_ARRIVALS <- NA
allPopularCarriers$OHARE_ARRIVALS <- NA

# Filter departures by only Midway and O'Hare Airports
for(i in 1:length(allPopularCarriers$CARRIER)) {
  top1_MID <- ILData2017 %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
  top1_MID = top1_MID %>% filter(ORIGIN_AIRPORT_ID == "Chicago Midway International")
  top1_MID = data.frame(summarize(group_by(top1_MID, CARRIER), sum(FR)))
  if (is.na(top1_MID$sum.FR.) || length(top1_MID$sum.FR.) == 0)
  {
    allPopularCarriers$MIDWAY_DEPARTURES[i] <- 0
  } else {
    allPopularCarriers$MIDWAY_DEPARTURES[i] <- top1_MID$sum.FR.
  }
  
  top1_OHARE <- ILData2017 %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
  top1_OHARE = top1_OHARE %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International")
  top1_OHARE = data.frame(summarize(group_by(top1_OHARE, CARRIER), sum(FR)))
  if (is.na(top1_OHARE$sum.FR.) || length(top1_OHARE$sum.FR.) == 0)
  {
    allPopularCarriers$OHARE_DEPARTURES[i] <- 0
  } else {
    allPopularCarriers$OHARE_DEPARTURES[i] <- top1_OHARE$sum.FR.
  }
}

# Filter arrivals by only Midway and O'Hare Airports
for(i in 1:length(allPopularCarriers$CARRIER)) {
  top2_MID <- ILData2017 %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
  top2_MID = top2_MID %>% filter(DEST_AIRPORT_ID == "Chicago Midway International")
  top2_MID = data.frame(summarize(group_by(top2_MID, CARRIER), sum(FR)))
  if (is.na(top2_MID$sum.FR.) || length(top2_MID$sum.FR.) == 0)
  {
    allPopularCarriers$MIDWAY_ARRIVALS[i] <- 0
  } else {
    allPopularCarriers$MIDWAY_ARRIVALS[i] <- top2_MID$sum.FR.
  }
  
  top2_OHARE <- ILData2017 %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
  top2_OHARE = top2_OHARE %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International")
  top2_OHARE = data.frame(summarize(group_by(top2_OHARE, CARRIER), sum(FR)))
  if (is.na(top2_OHARE$sum.FR.) || length(top2_OHARE$sum.FR.) == 0)
  {
    allPopularCarriers$OHARE_ARRIVALS[i] <- 0
  } else {
    allPopularCarriers$OHARE_ARRIVALS[i] <- top2_OHARE$sum.FR.
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
selectedData$WEATHER_DELAY<-ifelse(selectedData$WEATHER_DELAY>0,1,0)
selectedData$CARRIER_DELAY<-ifelse(selectedData$CARRIER_DELAY>0,1,0)
selectedData$NAS_DELAY<-ifelse(selectedData$NAS_DELAY>0,1,0)
selectedData$SECURITY_DELAY<-ifelse(selectedData$SECURITY_DELAY>0,1,0)
selectedData$LATE_AIRCRAFT_DELAY<-ifelse(selectedData$LATE_AIRCRAFT_DELAY>0,1,0)

carrierDelay <- subset(selectedData, CARRIER_DELAY > 0)
securityDelay <- subset(selectedData, SECURITY_DELAY > 0)


#count arrival delays per hour
hourlyDelayCount <- aggregate(cbind(count = delayTrue) ~ ARR_TIMEaggregated,
                              data = selectedData,
                              FUN = sum)

carrierDelayCount <- aggregate(cbind(carrier = CARRIER_DELAY) ~ ARR_TIMEaggregated,
                          data = selectedData,
                          FUN = sum)

weatherDelayCount <- aggregate(cbind(Weather = WEATHER_DELAY) ~ ARR_TIMEaggregated,
                               data = selectedData,
                               FUN = sum)

securityDelayCount <- aggregate(cbind(Weather = SECURITY_DELAY) ~ ARR_TIMEaggregated,
                               data = selectedData,
                               FUN = sum)

nasDelayCount <- aggregate(cbind(Weather = NAS_DELAY) ~ ARR_TIMEaggregated,
                                data = selectedData,
                                FUN = sum)

lateDelayCount <- aggregate(cbind(Weather = LATE_AIRCRAFT_DELAY) ~ ARR_TIMEaggregated,
                                data = selectedData,
                                FUN = sum)

#give niver column names
names(hourlyDelayCount) <- c("Hour", "Count")
names(carrierDelayCount) <- c("Hour", "Carrier")
names(weatherDelayCount) <- c("Hour", "Weather")
names(securityDelayCount) <- c("Hour", "Security")
names(nasDelayCount) <- c("Hour", "National Air System")
names(lateDelayCount) <- c("Hour", "Late Aircraft")

#create new table that will also hold percentage
totalselectedDataPercentage <- merge(totalselectedData,hourlyDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)
totalselectedDataPercentage <- merge(totalselectedDataPercentage,carrierDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)
totalselectedDataPercentage <- merge(totalselectedDataPercentage,weatherDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)
totalselectedDataPercentage <- merge(totalselectedDataPercentage,securityDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)
totalselectedDataPercentage <- merge(totalselectedDataPercentage,nasDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)
totalselectedDataPercentage <- merge(totalselectedDataPercentage,lateDelayCount,by="Hour", all.x= TRUE, all.y= TRUE)


totalselectedDataPercentage$Percentage <- (totalselectedDataPercentage$Weather / (totalselectedDataPercentage$Departures + totalselectedDataPercentage$Arrivals)) * 100

#drop arrivals and departures from table
#totalselectedDataPercentage <- subset(totalselectedDataPercentage, select = -c(2,3) )

#round percentage
totalselectedDataPercentage$Percentage <-round(totalselectedDataPercentage$Percentage, 0)

#give nicer column names
#names(totalselectedDataPercentage) <- c("Hour", "Total Delays", "% of selectedData")

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
  names(totalOrigin) <- c("Airport Name", "Arrivals")
  names(totalDest) <- c("Airport Name", "Departures")
  
  #now we combine the two totals togheter
  totalDepartures <- merge(totalDest,totalOrigin,by="Airport Name")
  totalDepartures$"Total Flights" <- totalDepartures$"Arrivals" +totalDepartures$"Departures"
  #last step is to sort by total count
  totalDepartures <- totalDepartures[order(-totalDepartures$"Total Flights"),]

  return(totalDepartures)
}

  
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
  
  #isabel outputs
  
  calculatedPercentage <- reactive({
    if ( "Security" %in% input$var) return((totalselectedDataPercentage$Security / (totalselectedDataPercentage$Departures + totalselectedDataPercentage$Arrivals)) * 100)
    if ( "Weather" %in% input$var) return((totalselectedDataPercentage$Weather / (totalselectedDataPercentage$Departures + totalselectedDataPercentage$Arrivals)) * 100)
  })
  
  
  add_to_df <- reactive({
    totalselectedDataPercentage$Percentage <- NA
    nRows <- nrow(totalselectedDataPercentage)
    totalselectedDataPercentage$Percentage <- calculatedPercentage()
    
    totalselectedDataPercentage
    
  })
  
  
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
    plot_ly(hourlyDepartures, x = ~hourlyDepartures$Hour, y = ~hourlyDepartures$Count, type = 'scatter', mode = 'lines', name = 'Departures', hoverinfo = 'text',
            text = ~paste('</br>', hourlyDepartures$Count, ' Departures </br>'), marker = list(color = 'rgb(49,130,189)')) %>%
      add_trace(x = ~hourlyArrivals$Hour, y = ~hourlyArrivals$Count, name = 'Arrivals', type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste('</br>', hourlyArrivals$Count, ' Arrivals </br>'),
                
                marker = list(color = '#ff7f0e')) %>%
      layout(xaxis = list(title = "Time Period", tickangle = -45),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  output$delayGraph <- renderPlotly({
    #if (v$data == 'Carrier') return()
    #cleanedData
    userInput <- input$delayButtons
    print(userInput)
    plot_ly(data =  totalselectedDataPercentage, x = ~totalselectedDataPercentage$Hour, y = ~get(input$delayButtons), type = "bar", showlegend=TRUE, hoverinfo = 'text',
            text = ~paste('</br>', Weather, ' Delays </br>',
                          Percentage, '% of Flights</br>'),
            marker=list(color=~totalselectedDataPercentage$Percentage, showscale=TRUE)) %>% layout(xaxis = list(title = "Time Period", tickangle = -45),yaxis = list(title = "# of Flights"),
                                                                                                   margin = list(b = 100),
                                                                                                   barmode = 'group')
    
  })
  
  #bart outputs
  
  # set up the margins for graphs
  graphMargins <- list(
    l = 50,
    r = 150,
    b = 200,
    t = 10,
    pad = 2
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
  
  createTop15Airports <- function(airport_name)
  {
    barChart <- renderPlotly({
      df <- airportTotals(airport_name)
      # get only the top 15 locations
      df <- df  %>% top_n(15)
      
      plot_ly(df, x = ~df$"Airport Name", y = ~df$"Departures", type = 'bar',name = 'Departures', text = paste("Total airport flights:" ,  (df$"Total Flights"))) %>%
        add_trace(y =  ~df$"Arrivals", name = 'Arrivals') %>%
        layout(xaxis = list(categoryorder = "array",categoryarray = df$"Airport Name", title = "Airport Name", tickangle = -45),
               yaxis = list(title = "Airport Flights"),
               barmode = 'stack',
               margin = graphMargins
        )
    })
    return(barChart)
  }
  #create the two different bar charts   
  output$bartChart1 <-createTop15Airports("Chicago O'Hare International")
  
  #output$bartChart2 <- createTop15Airports("Chicago Midway International")

  # bar chart of top carriers total departure and arrival in ohare and midway FOR DECEMBER 2017
  output$popularGraph <- renderPlotly({
    plot_ly(popularCarriers, x = ~popularCarriers$CARRIER, y = ~popularCarriers$MIDWAY_DEPARTURES, type = 'bar', name = 'Departures Midway',
            hoverinfo = 'text', text = ~paste('</br>', popularCarriers$MIDWAY_DEPARTURES, 'Departures Midway</br>'),
            marker = list(color = 'rgb(51,160,44)')) %>%
      
      add_trace(x = ~popularCarriers$CARRIER, y = ~popularCarriers$MIDWAY_ARRIVALS, name = 'Arrivals Midway', hoverinfo = 'text',
                text = ~paste('</br>', popularCarriers$MIDWAY_ARRIVALS, 'Arrivals Midway </br>'),
                marker = list(color = 'rgb(178,223,138)')) %>%
      
      add_trace(x = ~popularCarriers$CARRIER, y = ~popularCarriers$OHARE_DEPARTURES, name = 'Departures Ohare', hoverinfo = 'text',
                text = ~paste('</br>', popularCarriers$OHARE_DEPARTURES, ' Departures Ohare </br>'),
                marker = list(color = 'rgb(31,120,180)')) %>%
      
      add_trace(x = ~popularCarriers$CARRIER, y = ~popularCarriers$OHARE_ARRIVALS, name = 'Arrivals Ohare', hoverinfo = 'text',
                text = ~paste('</br>', popularCarriers$OHARE_ARRIVALS, 'Arrivals Ohare </br>'),
                marker = list(color = 'rgb(166,206,227)')) %>%
      
      layout(xaxis = list(title = "Carriers", tickangle = -45, categoryorder = "array", categoryarray = popularCarriers$CARRIER),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 130),
             barmode = 'group')
  })
  
  # obtained from the following example: https://plot.ly/r/range-slider/
  # table of top carriers total departure and arrival in ohare and midway FOR DECEMBER 2017
  output$topCarriers <- DT::renderDataTable(
    DT::datatable({
      popularCarriers
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE),
    colnames = c('TOTAL_FLIGHTS' = 3)
    )
  )
  
  # bar chart of departure and arrival PER weekday in ohare and midway FOR DECEMBER 2017
  output$weekdayGraph <- renderPlotly({
    plot_ly(flightsByWeekday, x = ~flightsByWeekday$Weekday, y = ~flightsByWeekday$MidwayDeparturesTotal, type = 'bar', name = 'Departures Midway',
            hoverinfo = 'text', text = ~paste('</br>', flightsByWeekday$MidwayDeparturesTotal, 'Departures Midway</br>'),
            marker = list(color = 'rgb(51,160,44)')) %>%
      
      add_trace(x = ~flightsByWeekday$Weekday, y = ~flightsByWeekday$MidwayArrivalsTotal, name = 'Arrivals Midway', hoverinfo = 'text',
                text = ~paste('</br>', flightsByWeekday$MidwayArrivalsTota, 'Arrivals Midway </br>'),
                marker = list(color = 'rgb(178,223,138)')) %>%
      
      add_trace(x = ~flightsByWeekday$Weekday, y = ~flightsByWeekday$OhareDeparturesTotal, name = 'Departures Ohare', hoverinfo = 'text',
                text = ~paste('</br>', flightsByWeekday$OhareDeparturesTotal, ' Departures Ohare </br>'),
                marker = list(color = 'rgb(31,120,180)')) %>%
      
      add_trace(x = ~flightsByWeekday$Weekday, y = ~flightsByWeekday$OhareArrivalsTotal, name = 'Arrivals Ohare', hoverinfo = 'text',
                text = ~paste('</br>', flightsByWeekday$OhareArrivalsTotal, 'Arrivals Ohare </br>'),
                marker = list(color = 'rgb(166,206,227)')) %>%
      
      layout(xaxis = list(title = "Weekday", tickangle = -45, categoryorder = "array", categoryarray = popularCarriers$CARRIER),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 130),
             barmode = 'group')
  })
  
  # This is for 'B' part of project. User is shown 12 months of data.
  output$allMonthsPopularGraph <- renderPlotly({
    plot_ly(allPopularCarriers, x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$MIDWAY_DEPARTURES, type = 'bar', name = 'Departures Midway',
            hoverinfo = 'text', text = ~paste('</br>', allPopularCarriers$MIDWAY_DEPARTURES, 'Departures Midway</br>'),
            marker = list(color = 'rgb(51,160,44)')) %>%
      
      add_trace(x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$MIDWAY_ARRIVALS, name = 'Arrivals Midway', hoverinfo = 'text',
                text = ~paste('</br>', allPopularCarriers$MIDWAY_ARRIVALS, 'Arrivals Midway </br>'),
                marker = list(color = 'rgb(178,223,138)')) %>%
      
      add_trace(x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$OHARE_DEPARTURES, name = 'Departures Ohare', hoverinfo = 'text',
                text = ~paste('</br>', allPopularCarriers$OHARE_DEPARTURES, ' Departures Ohare </br>'),
                marker = list(color = 'rgb(31,120,180)')) %>%
      
      add_trace(x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$OHARE_ARRIVALS, name = 'Arrivals Ohare', hoverinfo = 'text',
                text = ~paste('</br>', allPopularCarriers$OHARE_ARRIVALS, 'Arrivals Ohare </br>'),
                marker = list(color = 'rgb(166,206,227)')) %>%
      
      layout(xaxis = list(title = "Carriers", tickangle = -45, categoryorder = "array", categoryarray = popularCarriers$CARRIER),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 130),
             barmode = 'group')
  })
  
  # associated data table for user is shown 12 months of data.
  output$allMonthsTopCarriersTable <- DT::renderDataTable(
    DT::datatable({
      allPopularCarriers
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE)
    )
  )
  
  # This is for 'A' part of project. User selects airport they want to view data for --Vijay
  output$specificAirportPlot <- renderPlotly({
    allPopularCarriers$S_DEPARTURES <- NA
    allPopularCarriers$S_ARRIVALS <- NA
    
    # Filter departures by Specified airport
    for(i in 1:length(allPopularCarriers$CARRIER)) {
      top1_S <- cleanedFlights %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
      top1_S = top1_S %>% filter(ORIGIN_AIRPORT == input$airport-dropdown)
      top1_S = data.frame(summarize(group_by(top1_S, CARRIER), sum(FR)))
      if (is.na(top1_S$sum.FR.) || length(top1_S$sum.FR.) == 0)
      {
        allPopularCarriers$S_DEPARTURES[i] <- 0
      } else {
        allPopularCarriers$S_DEPARTURES[i] <- top1_S$sum.FR.
      }
    }
    
    # Filter arrivals by only Specified airport
    for(i in 1:length(allPopularCarriers$CARRIER)) {
      top2_S <- cleanedFlights %>% filter(CARRIER == allPopularCarriers$CARRIER[i])
      top2_S = top2_S %>% filter(DEST_AIRPORT == input$airport-dropdown)
      top2_S = data.frame(summarize(group_by(top2_MID, CARRIER), sum(FR)))
      if (is.na(top2_S$sum.FR.) || length(top2_S$sum.FR.) == 0)
      {
        allPopularCarriers$S_ARRIVALS[i] <- 0
      } else {
        allPopularCarriers$S_ARRIVALS[i] <- top2_S$sum.FR.
      }
    }
    
    plot_ly(allPopularCarriers, x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$S_DEPARTURES, type = 'bar', name = 'Departures',
            hoverinfo = 'text', text = ~paste('</br>', allPopularCarriers$S_DEPARTURES, 'Departures</br>'),
            marker = list(color = 'rgb(51,160,44)')) %>%
      
      add_trace(x = ~allPopularCarriers$CARRIER, y = ~allPopularCarriers$S_ARRIVALS, name = 'Arrivals', hoverinfo = 'text',
                text = ~paste('</br>', allPopularCarriers$S_ARRIVALS, 'Arrivals </br>'),
                marker = list(color = 'rgb(178,223,138)')) %>%
      
      layout(xaxis = list(title = "Carriers", tickangle = -45, categoryorder = "array", categoryarray = popularCarriers$CARRIER),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 130),
             barmode = 'group')
  })
  
  # table of total departure and arrival PER weekday in ohare and midway
  output$weekdayTable <- DT::renderDataTable(
    DT::datatable({
      flightsByWeekday
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE)
    )
  )
}

