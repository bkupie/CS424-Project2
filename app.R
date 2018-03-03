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

#put the server/ui into a seperate file 
source('server.R', local = TRUE)
source('ui.R', local = TRUE)
#start the actual application
shinyApp(ui = ui, server = server)