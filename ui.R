library(shiny)
library(RJSONIO)

jsonFile <- "https://tst-sport.trifork.nl/api/floors"
JSONdata <- fromJSON(jsonFile)

# extract the data node
floors<-JSONdata[[2]]


name <- (JSONdata$name)

shinyUI(fluidPage(
  titlePanel("Smartfloor E - Steps"),
  sidebarLayout(
    sidebarPanel(("sidebarPanel"),
                 textInput("name", "Enter your name", ""),
                 textInput("age", "Enter your age", "")),
    mainPanel(("mainPanel"), floors)
  )
))