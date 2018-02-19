# Project 1 for CS 424 Spring 2018 UIC 

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
require(dplyr)

# start up the gui 
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bart", tabName = "outbreaks", icon = icon("ambulance", lib = "font-awesome")),
      menuItem("Isabel", tabName = "outbreaks", icon = icon("ambulance", lib = "font-awesome")),
      menuItem("Vijay", tabName = "outbreaks", icon = icon("ambulance", lib = "font-awesome"))
      
    )
  ),
  
  #start of the body 
  dashboardBody(
    
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
}  
  
#start the actual application 
shinyApp(ui = ui, server = server)
