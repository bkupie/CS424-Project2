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
library(dplyr)

# process dataset here
flights <- read.table(file = "ontime_flights.cleaned.csv", sep = ",", header = TRUE)

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
              h2("Isabel tab content")
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
  
  #bart outputs 
  #render the table for 
  output$bartTable1 <- DT::renderDataTable(
    DT::datatable({ 
      #only secelect origin and destination IDs 
      df <- subset( flights, select = c(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID) )
      #counted = count(df, c('ORIGIN_AIRPORT_ID'))
      #show only the top 15 
      top15 = df[sample(nrow(df), 15), ]  
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(searching = FALSE, pageLength = 5, lengthChange = TRUE, order = list(list(0, 'asc')))
  )
  )
}  
  
#start the actual application 
shinyApp(ui = ui, server = server)
