server <- function(input, output) {
  
  # The variable month_data = 12 months of data in IL in an array (i.e. january == month_data[[1]])
  load("rdata/month_data.RData")
  
  # The object below just binds all 12 months worth of data in IL into ONE R object
  load("rdata/ILData2017.RData")
  
  # rank the TOP CARRIERS across 12 months and filters it to Ohare and Midway (seperate columns)
  load("rdata/allPopularCarriers.RData")
  
  # rank the TOP AIRPORTS across 12 months
  load("rdata/allPopularAirports.RData")
  
  #load the data that will be used to create the map
  load("rdata/FlightMap.rdata")
  
  #load data for the heatmap
  load("rdata/topForMonths.RData")
  load("rdata/top15Airports.RData")
  
  #load data for yearly delays graph
  load("rdata/yearlyDelays.RData")
  
  #load data for yearly arrivals and departures heatmap
  load("rdata/hourlyYearlyData.RData")
  
  #load data for the top 50 airports
  
  #continue this method into am/pm formatting
  #ILData2017$DEP_TIMEampm <- as.POSIXct(sprintf("%04.0f", ILData2017$DEP_TIME), format='%H%M')
  #ILData2017$DEP_TIMEampm <- cut(ILData2017$DEP_TIMEampm, breaks = "hour")
  #ILData2017$DEP_TIMEampm <- substr(ILData2017$DEP_TIMEampm, 12, 16)
  #ILData2017$DEP_TIMEampm <- format(strptime(ILData2017$DEP_TIMEampm,format ='%H:%M'), "%I:%M %p")

  # selectedData$ARR_TIMEampm <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
  # selectedData$ARR_TIMEampm <- cut(selectedData$ARR_TIMEampm, breaks = "hour")
  # selectedData$ARR_TIMEampm <- substr(selectedData$ARR_TIMEampm, 12, 16)
  # ILData2017$ARR_TIMEampm <- format(strptime(ILData2017$ARR_TIMEampm,format ='%H:%M'), "%I:%M %p")
  
  #create a list for the AM/PM time 
  time <- c("12:00 AM","1:00 AM","2:00 AM","3:00 AM","4:00 AM","5:00 AM","6:00 AM","7:00 AM","8:00 AM","9:00 AM","10:00 AM","11:00 AM",
            "12:00 PM","1:00 PM","2:00 PM","3:00 PM","4:00 PM","5:00 PM","6:00 PM","7:00 PM","8:00 PM","9:00 PM","10:00 PM","11:00 PM")
  timeampm <- data_frame(time)
  time <- c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00",
               "13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
  timereg <- data_frame(time)
  
  # reactive elements are here ============================================================================
 getTimeFrame <- reactive({
   if(input$time)
   {selectedTime <- timereg}
   else if(!input$time)
   {selectedTime <- timeampm}  
   selectedTime
   })

  
   chosenMonth <- reactive({
    as.numeric(input$"month-select")
  })
  
  # selectedData based on chosen Month
  sData <- reactive({
    selectedData <- month_data[[chosenMonth()]]
    
    selectedData <- selectedData %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International" ||ORIGIN_AIRPORT_ID == "Chicago Midway International" || DEST_AIRPORT_ID == "Chicago O'Hare International" ||DEST_AIRPORT_ID == "Chicago Midway International")

    #create new column that converts minutes to hour:minute
    selectedData$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$DEP_TIME), format='%H%M')
    selectedData$DEP_TIMEaggregated <- cut(selectedData$DEP_TIMEaggregated, breaks = "hour")
    selectedData$DEP_TIMEaggregated <- substr(selectedData$DEP_TIMEaggregated, 12, 16)
    
    selectedData$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
    selectedData$ARR_TIMEaggregated <- cut(selectedData$ARR_TIMEaggregated, breaks = "hour")
    selectedData$ARR_TIMEaggregated <- substr(selectedData$ARR_TIMEaggregated, 12, 16)
    
    #add int boolean for if delay exists or not
    selectedData$delayTrue<-ifelse(selectedData$ARR_DELAY_NEW>0 | selectedData$DEP_DELAY_NEW > 0,1,0)
    selectedData$WEATHER_DELAY<-ifelse(selectedData$WEATHER_DELAY>0,1,0)
    selectedData$CARRIER_DELAY<-ifelse(selectedData$CARRIER_DELAY>0,1,0)
    selectedData$NAS_DELAY<-ifelse(selectedData$NAS_DELAY>0,1,0)
    selectedData$SECURITY_DELAY<-ifelse(selectedData$SECURITY_DELAY>0,1,0)
    selectedData$LATE_AIRCRAFT_DELAY<-ifelse(selectedData$LATE_AIRCRAFT_DELAY>0,1,0)
    
    selectedData
  })
  
  # totalSelectedData based on chosen Month
  tsData <- reactive({
    selectedData <- sData()
    
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
    
    # merge into total selectedData for both departure and arrival
    totalselectedData <- merge(hourlyDepartures,hourlyArrivals,by="Hour")
    
    #give nicer column names
    names(totalselectedData) <- c("Hour", "Departures", "Arrivals")
    
    totalselectedData
  })
  
  # totalSelectedDataPercentage based on chosen Month
  tsdpData <- reactive({
    selectedData <- sData()
    totalselectedData <- tsData()
    
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
    names(hourlyDelayCount) <- c("Hour", "Total Flights")
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
    
    userInput <- input$delayButtons
    print(userInput)
    
  #~get(input$delayButtons)
    totalselectedDataPercentage$Percentage <- (totalselectedDataPercentage[[userInput]] / (totalselectedDataPercentage$Departures + totalselectedDataPercentage$Arrivals)) * 100
    
    #drop arrivals and departures from table
    totalselectedDataPercentage <- subset(totalselectedDataPercentage, select = -c(2,3) )
    
    #round percentage
    totalselectedDataPercentage$Percentage <-round(totalselectedDataPercentage$Percentage, 0)
    
    #give nicer column names
    #names(totalselectedDataPercentage) <- c("Hour", "Total Delays", "% of selectedData")
    totalselectedDataPercentage
  })
  
  add_to_df <- reactive({
    totalselectedDataPercentage <- tsdpData()
    
    totalselectedDataPercentage$Percentage <- NA
    nRows <- nrow(totalselectedDataPercentage)
    totalselectedDataPercentage$Percentage <- calculatedPercentage()
    
    totalselectedDataPercentage
  })
  
  fbwData <- reactive({
    cleanedFlights <- month_data[[chosenMonth()]]
    
    # ordering flights by day of the week (i.e. how many on all the mondays of the month and so forth)
    # handy solution on how to group by days of the week found here: https://stackoverflow.com/questions/27828850/dplyr-does-not-group-data-by-date
    # below we generate a column that has specific weekday of the flight date (i.e. 12/1/17 = Friday)
    orderByWeekday <- cleanedFlights %>% 
      mutate(
        FL_DATE = parse_date_time(FL_DATE,"%y/%m/%d"),
        Weekday = wday(FL_DATE, label=TRUE, abbr=FALSE)
      )
    
    midwayDeparturesByWeekday = orderByWeekday %>% filter(ORIGIN_AIRPORT_ID == "Chicago Midway International") %>% group_by(Weekday) %>% summarise(Total = n())
    ohareDeparturesByWeekday = orderByWeekday %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International") %>% group_by(Weekday) %>% summarise(Total = n())
    
    midwayArrivalsByWeekday = orderByWeekday %>% filter(DEST_AIRPORT_ID == "Chicago O'Hare International") %>% group_by(Weekday) %>% summarise(Total = n())
    ohareArrivalsByWeekday = orderByWeekday %>% filter(DEST_AIRPORT_ID == "Chicago Midway International") %>% group_by(Weekday) %>% summarise(Total = n())
    
    flightsByWeekday <- data.frame(
      Weekday = midwayDeparturesByWeekday$Weekday,
      MidwayDeparturesTotal = midwayDeparturesByWeekday$Total,
      OhareDeparturesTotal = ohareDeparturesByWeekday$Total,
      MidwayArrivalsTotal = midwayArrivalsByWeekday$Total,
      OhareArrivalsTotal = ohareArrivalsByWeekday$Total
    )
    
    flightsByWeekday
  })
  
  pcData <- reactive({
    # ordering flights by most common airlines
    # based closely on following tutorial: https://rstudio-pubs-static.s3.amazonaws.com/52879_eaa8e7a9919b4bb6a2cf6e2bda587cb1.html
    cleanedFlights <- month_data[[chosenMonth()]]
    cleanedFlights$FR <- 1
    
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
      top1_MID = top1_MID %>% filter(ORIGIN_AIRPORT_ID == "Chicago Midway International")
      top1_MID = data.frame(summarize(group_by(top1_MID, CARRIER), sum(FR)))
      if (is.na(top1_MID$sum.FR.) || length(top1_MID$sum.FR.) == 0)
      {
        popularCarriers$MIDWAY_DEPARTURES[i] <- 0
      } else {
        popularCarriers$MIDWAY_DEPARTURES[i] <- top1_MID$sum.FR.
      }
      
      top1_OHARE <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
      top1_OHARE = top1_OHARE %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International")
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
      top2_MID = top2_MID %>% filter(DEST_AIRPORT_ID == "Chicago Midway International")
      top2_MID = data.frame(summarize(group_by(top2_MID, CARRIER), sum(FR)))
      if (is.na(top2_MID$sum.FR.) || length(top2_MID$sum.FR.) == 0)
      {
        popularCarriers$MIDWAY_ARRIVALS[i] <- 0
      } else {
        popularCarriers$MIDWAY_ARRIVALS[i] <- top2_MID$sum.FR.
      }
      
      top2_OHARE <- cleanedFlights %>% filter(CARRIER == popularCarriers$CARRIER[i])
      top2_OHARE = top2_OHARE %>% filter(DEST_AIRPORT_ID == "Chicago O'Hare International")
      top2_OHARE = data.frame(summarize(group_by(top2_OHARE, CARRIER), sum(FR)))
      if (is.na(top2_OHARE$sum.FR.) || length(top2_OHARE$sum.FR.) == 0)
      {
        popularCarriers$OHARE_ARRIVALS[i] <- 0
      } else {
        popularCarriers$OHARE_ARRIVALS[i] <- top2_OHARE$sum.FR.
      }
    }
    
    popularCarriers
  })
  
  mData <- reactive({
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    yearOfData <- list(" 2017")
    cMonth <- paste(textOfMonth[[as.numeric(input$"month-select")]], yearOfData[[1]])
    
    cMonth
  })
  
  wData <- reactive({
    textOfWeekday <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    yearOfData <- list(" in 2017")
    cWeekday <- paste(textOfWeekday[[as.numeric(input$"weekday-select")]], yearOfData[[1]])
    
    cWeekday
  })
  
  sCarrierData <- reactive({
    selectedData <- ILData2017 %>% filter(FL_DATE == as.character(input$"date-selectCarrier")) %>% filter(CARRIER == as.character(input$"airline-dropdown"))
    
    selectedData <- selectedData %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International" ||ORIGIN_AIRPORT_ID == "Chicago Midway International" || DEST_AIRPORT_ID == "Chicago O'Hare International" ||DEST_AIRPORT_ID == "Chicago Midway International")
    
    #create new column that converts minutes to hour:minute
    selectedData$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$DEP_TIME), format='%H%M')
    selectedData$DEP_TIMEaggregated <- cut(selectedData$DEP_TIMEaggregated, breaks = "hour")
    selectedData$DEP_TIMEaggregated <- substr(selectedData$DEP_TIMEaggregated, 12, 16)
    
    selectedData$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
    selectedData$ARR_TIMEaggregated <- cut(selectedData$ARR_TIMEaggregated, breaks = "hour")
    selectedData$ARR_TIMEaggregated <- substr(selectedData$ARR_TIMEaggregated, 12, 16)
    
    selectedData
  })
  
  sCarrierYearData <- reactive({
    selectedData <- ILData2017 %>% filter(CARRIER == as.character(input$"airline-dropdown"))
    selectedData <- selectedData %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International" ||ORIGIN_AIRPORT_ID == "Chicago Midway International" || DEST_AIRPORT_ID == "Chicago O'Hare International" ||DEST_AIRPORT_ID == "Chicago Midway International")
    
    selectedData
  })
  
  sWeekday <- reactive({
    selectedData <- ILData2017 %>% filter(DAY_OF_WEEK == as.character(input$"weekday-select"))
    
    selectedData <- selectedData %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International" ||ORIGIN_AIRPORT_ID == "Chicago Midway International" || DEST_AIRPORT_ID == "Chicago O'Hare International" ||DEST_AIRPORT_ID == "Chicago Midway International")
    
    #create new column that converts minutes to hour:minute
    selectedData$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$DEP_TIME), format='%H%M')
    selectedData$DEP_TIMEaggregated <- cut(selectedData$DEP_TIMEaggregated, breaks = "hour")
    selectedData$DEP_TIMEaggregated <- substr(selectedData$DEP_TIMEaggregated, 12, 16)
    
    selectedData$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
    selectedData$ARR_TIMEaggregated <- cut(selectedData$ARR_TIMEaggregated, breaks = "hour")
    selectedData$ARR_TIMEaggregated <- substr(selectedData$ARR_TIMEaggregated, 12, 16)
    
    selectedData
  })
  
  sDelayWeekdayYear <- reactive({
    selectedData <- ILData2017 %>% filter(DAY_OF_WEEK == as.character(input$"weekday-select"))
    
    selectedData <- selectedData %>% filter(ORIGIN_AIRPORT_ID == "Chicago O'Hare International" ||ORIGIN_AIRPORT_ID == "Chicago Midway International" || DEST_AIRPORT_ID == "Chicago O'Hare International" ||DEST_AIRPORT_ID == "Chicago Midway International")
    
    #create new column that converts minutes to hour:minute
    selectedData$DEP_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$DEP_TIME), format='%H%M')
    selectedData$DEP_TIMEaggregated <- cut(selectedData$DEP_TIMEaggregated, breaks = "hour")
    selectedData$DEP_TIMEaggregated <- substr(selectedData$DEP_TIMEaggregated, 12, 16)
    
    selectedData$ARR_TIMEaggregated <- as.POSIXct(sprintf("%04.0f", selectedData$ARR_TIME), format='%H%M')
    selectedData$ARR_TIMEaggregated <- cut(selectedData$ARR_TIMEaggregated, breaks = "hour")
    selectedData$ARR_TIMEaggregated <- substr(selectedData$ARR_TIMEaggregated, 12, 16)
    
    #add int boolean for if delay exists or not
    selectedData$delayTrue<-ifelse(selectedData$ARR_DELAY_NEW>0 | selectedData$DEP_DELAY_NEW > 0,1,0)
    selectedData$WEATHER_DELAY<-ifelse(selectedData$WEATHER_DELAY>0,1,0)
    selectedData$CARRIER_DELAY<-ifelse(selectedData$CARRIER_DELAY>0,1,0)
    selectedData$NAS_DELAY<-ifelse(selectedData$NAS_DELAY>0,1,0)
    selectedData$SECURITY_DELAY<-ifelse(selectedData$SECURITY_DELAY>0,1,0)
    selectedData$LATE_AIRCRAFT_DELAY<-ifelse(selectedData$LATE_AIRCRAFT_DELAY>0,1,0)
    
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
    
    # merge into total selectedData for both departure and arrival
    totalselectedData <- merge(hourlyDepartures,hourlyArrivals,by="Hour")
    
    #give nicer column names
    names(totalselectedData) <- c("Hour", "Departures", "Arrivals")
    
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
    totalselectedDataPercentage
  })
  
  # Output Graphs and Visualizations =========================================================================
  # Actual visualizations of the data is done below. We have many reactive variables which are all defined 
  # above. We used various naming conventions so I tried to centralize it. -Vijay
  # ==========================================================================================================
  # Graph presets for "readable" content =====================================================================
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
  
  # set up the margins for graphs
  graphMargins <- list(
    l = 50,
    r = 200,
    b = 220,
    t = 10,
    pad = 2
  )
  
  heatMargins <- list(
    l = 350,
    r = 50,
    b = 50,
    t = 10,
    pad = 2
  )
  
  # Graphs ==================================================================================================
  output$hourlyGraph <- renderPlotly({
    selectedData <- sData()  
    timeFrame <- getTimeFrame()
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
    
    plot_ly(hourlyDepartures, x = ~timeFrame$time, y = ~hourlyDepartures$Count, type = 'scatter', mode = 'lines', name = 'Departures', 
            hoverinfo = 'text', text = ~paste('</br>', hourlyDepartures$Count, ' Departures </br>'), 
            marker = list(color = 'rgb(49,130,189)')) %>%
      
      add_trace(x = ~timeFrame$time, y = ~hourlyArrivals$Count, name = 'Arrivals', type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste('</br>', hourlyArrivals$Count, ' Arrivals </br>'),
                marker = list(color = '#ff7f0e')) %>%
      
      layout(xaxis = list(title = "Time Period", tickangle = -45,categoryorder = "array",categoryarray = timeFrame$time),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  output$hourlyYearGraphArr <- renderPlotly({
      data <- hourlyYearlyData
      timeFrame <- getTimeFrame()
      plot_ly(x= data$Month,y= data$Time, z = data$Arrivals, type = "heatmap",hoverinfo = 'text',
              text = ~paste('</br> Departures: ', data$Departures, '</br> Month: ', data$Month, '</br> Time: ', timeFrame$time ))%>%
        layout(xaxis = list(title = "Month", autotick = F, dtick = 1))%>%
        layout(xaxis = list(title = "Month", autotick = F, dtick = 1),
               yaxis = list(categoryorder = "array",categoryarray = timeFrame$time))
    })
  
  output$hourlyYearGraphDep <- renderPlotly({
      data <- hourlyYearlyData
      timeFrame <- getTimeFrame()
      plot_ly(x= data$Month,y= data$Time, z = data$Departures, type = "heatmap", hoverinfo = 'text',
              text = ~paste('</br> Departures: ', data$Departures, '</br> Month: ', data$Month, '</br> Time: ', timeFrame$time ))%>%
        layout(xaxis = list(title = "Month", autotick = F, dtick = 1),
               yaxis = list(title = "Time",categoryorder = "array",categoryarray = timeFrame$time))

    })
  
  output$yearlyDelaysGraph <- renderPlotly({
      data <- yearlyDelays
    
      plot_ly(data, x = ~Month, y = ~Carrier, type = 'bar', name = 'Carrier Delay') %>%
        add_trace(y = ~Weather, name = 'Weather Delay') %>% add_trace(y = ~Security, name = 'Security Delay') %>% 
        add_trace(y = ~`National Air System`, name = 'National Air System Delay') %>% 
        add_trace(y = ~`Late Aircraft`, name = 'Late Aircraft Delay') %>%
        layout(title = "Total Delays in 2017", xaxis = list(title = "Month", autotick = F, dtick = 1)) %>%
        layout(yaxis = list(title = 'Count'), barmode = 'stack')
    })
  
  output$delayGraph <- renderPlotly({
    totalselectedDataPercentage <- tsdpData()
    timeFrame <- getTimeFrame()
    userInput <- input$delayButtons
    monthChoice <- chosenMonth()
    
    
    #newTitle <- userInput + " Delays in Month"
    
    plot_ly(data =  totalselectedDataPercentage, x = ~timeFrame$time, y = ~get(input$delayButtons), 
            type = "bar", showlegend=TRUE, hoverinfo = 'text', 
            text = ~paste('</br>', Weather, ' Delays </br>', Percentage, '% of Flights</br>'), 
            marker=list(color=~totalselectedDataPercentage$Percentage, showscale=TRUE)) %>%
      
      layout(title = paste("Hourly", userInput, "Delays in", month.abb[monthChoice],"2017", sep=" "), xaxis = list(title = "Time Period", tickangle = -45,categoryorder = "array",categoryarray = timeFrame$time),yaxis = list(title = "# of Flights"),
             margin = list(b = 100), barmode = 'group')
  })
  
  
  # bar chart of top carriers total departure and arrival in ohare and midway FOR CHOSEN MONTH
  output$popularGraph <- renderPlotly({
    popularCarriers <- pcData()
    
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
  
  # bar chart of departure and arrival PER weekday in ohare and midway FOR CHOSEN MONTH
  output$weekdayGraph <- renderPlotly({
    flightsByWeekday <- fbwData()
    popularCarriers <- pcData()
    
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
  
  # chart of dep/arr in ohare and midway FOR CHOSEN WEEKDAY ACROSS THE YEAR
  output$specificWeekdayYearPlot <- renderPlotly({
    selectedData <- sWeekday()
    timeFrame <- getTimeFrame()
    #BARTT
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
    
    plot_ly(hourlyDepartures, x = ~timeFrame$time, y = ~hourlyDepartures$Count, type = 'scatter', mode = 'lines', name = 'Departures', 
            hoverinfo = 'text', text = ~paste('</br>', hourlyDepartures$Count, ' Departures </br>'), 
            marker = list(color = 'rgb(49,130,189)')) %>%
      
      add_trace(x = ~timeFrame$time, y = ~hourlyArrivals$Count, name = 'Arrivals', type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste('</br>', hourlyArrivals$Count, ' Arrivals </br>'),
                marker = list(color = '#ff7f0e')) %>%
      
      layout(xaxis = list(title = "Time Period", tickangle = -45,categoryorder = "array",categoryarray = timeFrame$time),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  # chart of total delays in ohare and midway FOR CHOSEN WEEKDAY ACROSS THE YEAR
  output$specificWeekdayDelayPlot <- renderPlotly({
    totalselectedDataPercentage <- sDelayWeekdayYear()
    timeFrame <- getTimeFrame()
    
    userInput <- input$delayButtons2

    plot_ly(data =  totalselectedDataPercentage, x = ~timeFrame$time, y = ~get(input$delayButtons2), 
            type = "bar", showlegend=TRUE, hoverinfo = 'text', 
            text = ~paste('</br>', Weather, ' Delays </br>', Percentage, '% of Flights</br>'), 
            marker=list(color=~totalselectedDataPercentage$Percentage, showscale=TRUE)) %>%
      
      layout(xaxis = list(title = "Time Period", tickangle = -45,categoryorder = "array",categoryarray = timeFrame$time),yaxis = list(title = "# of Flights"),
             margin = list(b = 100), barmode = 'group')
  })
  
  # This is for 'B' part of project. User is top carriers total dep/arr in ohare and midway FOR YEAR
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
      
      layout(xaxis = list(title = "Carriers", tickangle = -45, categoryorder = "array", categoryarray = allPopularCarriers$CARRIER),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 130),
             barmode = 'group')
  })
  
  # This is for 'A' part of project. User selects CARRIER and DATE --> generates total dep/arr per hour graph
  output$specificCarrier24Plot <- renderPlotly({
    selectedData <- sCarrierData()
    timeFrame <- getTimeFrame()
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
    
    plot_ly(hourlyDepartures, x = ~hourlyDepartures$Hour, y = ~hourlyDepartures$Count, type = 'scatter', mode = 'lines', name = 'Departures', 
            hoverinfo = 'text', text = ~paste('</br>', hourlyDepartures$Count, ' Departures </br>'), 
            marker = list(color = 'rgb(49,130,189)')) %>%
      
      add_trace(x = ~hourlyArrivals$Hour, y = ~hourlyArrivals$Count, name = 'Arrivals', type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste('</br>', hourlyArrivals$Count, ' Arrivals </br>'),
                marker = list(color = '#ff7f0e')) %>%

      layout(xaxis = list(title = "Time Period", tickangle = -45),#categoryorder = "array",categoryarray = timeFrame$time),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  # This is for 'A' part of project. User selects CARRIER --> generates total dep/arr per hour graph
  output$specificCarrierYearPlot <- renderPlotly({
    selectedData <- sCarrierYearData()
    
    #count based on hour
    departures <- aggregate(cbind(count = CARRIER) ~ FL_DATE,
                                  data = selectedData,
                                  FUN = function(x){NROW(x)})
    
    arrivals <- aggregate(cbind(count = CARRIER) ~ FL_DATE,
                                data = selectedData,
                                FUN = function(x){NROW(x)})
    
    #add nicer names to columns
    names(departures) <- c("Date", "Count")
    names(arrivals) <- c("Date", "Count")
    
    plot_ly(departures, x = ~departures$Date, y = ~departures$Count, type = 'scatter', mode = 'lines', name = 'Departures', 
            hoverinfo = 'text', text = ~paste('</br>', departures$Count, ' Departures </br>'), 
            marker = list(color = 'rgb(49,130,189)')) %>%
      
      add_trace(x = ~arrivals$Date, y = ~arrivals$Count, name = 'Arrivals', type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste('</br>', arrivals$Count, ' Arrivals </br>'),
                marker = list(color = '#ff7f0e')) %>%
      
      layout(xaxis = list(title = "Time Period", tickangle = -45),
             yaxis = list(title = "# of Flights"),
             margin = list(b = 100),
             barmode = 'group')
  })
  
  airportTotals <- function(airport_name) {
    selectedData <- sData()
    
    #count locations based on amount of origin
    totalOrigin <- selectedData %>% filter(DEST_AIRPORT_ID == airport_name)
    totalOrigin <- aggregate(cbind(count = ORIGIN_AIRPORT_ID) ~ ORIGIN_AIRPORT_ID,
                             data = totalOrigin,
                             FUN = function(x){NROW(x)})
    
    #count locations based on amount of destination
    totalDest <- selectedData %>% filter(ORIGIN_AIRPORT_ID == airport_name)
    totalDest <- aggregate(cbind(count = DEST_AIRPORT_ID) ~ DEST_AIRPORT_ID,
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
  
  createTop15Airports <- function(airport_name) {
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
  output$top15Chart1 <-createTop15Airports("Chicago O'Hare International")
  
  output$top15Chart2 <- createTop15Airports("Chicago Midway International")
  
  #create the top 15 airports over the 12 months 
  output$top15Airports12months <- renderPlotly({
    
    # create the graph now 
    plot_ly(topForMonths, x = ~Month, y = topForMonths$Airport, z= ~topForMonths$Frequency,colors = colorRamp(c("white","blue","black")), type = "heatmap")%>%
      colorbar(title = "Departures+Arrivals per month") %>%
      layout(yaxis = list(categoryorder = "array",categoryarray = top15Airports$Airport), xaxis = list( dtick = 1), margin = heatMargins)
    
  })
  
  #Create the map
  output$FLightMap <- renderPlotly({
    l <- list(color = toRGB("white"), width = 2)
    
    #On hover, give more information
    FlightMap$Hover <- with(FlightMap, paste(State, '<br>', "Origin Freq.", Origin.Freq, " | Dest Freq. : ", Dest.Freq,
                                             "<br>","Origin + Dest :", Sum.Freq, "| Total percentage", Percent , "%", "<br>"))
    #focus only on the USA 
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    #create the actual map
    p <- plot_geo(FlightMap, locationmode = 'USA-states') %>%
      add_trace(
        z = ~FlightMap$Percent, text = ~FlightMap$Hover, locations = ~FlightMap$State,
        color = ~FlightMap$Percent, colors = 'Purples'
      ) %>%
      colorbar(title = "Percent") %>% #name the bar properly 
      layout(
        title = '2017 Flights To/From Illinois <br>(Hover for breakdown)', #set the title 
        geo = g #focus on USA 
      )
    
  })
  
  output$monthText <- renderText({ mData() })
  
  output$carrierText <- renderText({ paste(as.character(input$"airline-dropdown"), "on ", as.character(input$"date-selectCarrier"))})
  
  output$carrierText2 <- renderText({ paste(as.character(input$"airline-dropdown"), "January - December 2017")})
  
  output$weekdayText <- renderText({ paste("Departures/Arrivals for ", wData()) })
  
  output$weekdayText2 <- renderText({ paste("Delays for", wData()) })
  
  output$mWeekdayText <- renderText({ paste("Departures/Arrivals by Weekday for ", mData())})
  
  # Tables ===================================================================================================
  output$totalselectedDataTable <- renderDataTable(tsData(), extensions = 'Scroller', 
    rownames = FALSE, options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      bFilter=0
    )
  )
  
  output$totalselectedDataPercentageTable <- renderDataTable(tsdpData(), extensions = 'Scroller', 
    rownames = FALSE, options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      bFilter=0
    )
  )
  
  # associated data table for user is shown 12 months of data.
  output$allMonthsTopCarriersTable <- DT::renderDataTable(
    DT::datatable({
      allPopularCarriers
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE)
    )
  )
  
  # obtained from the following example: https://plot.ly/r/range-slider/
  # table of top carriers total departure and arrival in ohare and midway FOR DECEMBER 2017
  output$topCarriersTable <- renderDataTable(pcData(), extensions = 'Scroller', 
    rownames = FALSE, options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      bFilter=0
    ), colnames = c('TOTAL_FLIGHTS' = 3)
  )
  
  # table of total departure and arrival PER weekday in ohare and midway
  output$weekdayTable <- renderDataTable(fbwData(), extensions = 'Scroller', 
    rownames = FALSE, options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      bFilter=0
    )
  )
  
  #render the table for departure/arrival counters
  output$top15Table1 <- DT::renderDataTable(DT::datatable({
    #show only the top 15
    df <- airportTotals("Chicago O'Hare International")
    head(df,15)
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE)
  )
  )
  
  output$top15Table2 <- DT::renderDataTable(DT::datatable({
    #show only the top 15
    df <- airportTotals("Chicago Midway International")
    head(df,15)
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE)
  )
  )
}
