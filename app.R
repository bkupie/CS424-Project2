# Project 2 for CS 424 Spring 2018 UIC
# Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae

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
library(data.table)

#put the server/ui into a seperate file 
source('ui.R', local = TRUE)
source('server.R', local = TRUE)


#start the actual application
shinyApp(ui = ui, server = server)