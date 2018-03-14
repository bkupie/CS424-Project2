# Project 2 for CS 424 Spring 2018 UIC
# Authors: Vijayraj Mahida, Bartosz Kupiec, and Isabel Lindmae

#before we start anything, we want to make sure our project workspace is nice and clean
#we will achieve this by removing any lingering packages from past runs 
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

#run the function
detachAllPackages()

#put the server/ui into a seperate file 
source('ui.R', local = TRUE)
source('server.R', local = TRUE)


#start the actual application
shinyApp(ui = ui, server = server)