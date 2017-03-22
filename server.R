library(shiny)
library(RJSONIO)


shinyServer(function(input, output, session) {
  observeEvent(input$button, {
    
    #hier nog werk nodig, geeft nu enkel airtime van eerste stap terug:
    a = "https://tst-sport.trifork.nl/api/footstep/session/588a04cd2ab79c000531c18e/user/"
    b <- sub("\\).*", "", sub(".*\\(", "", input$dataset))
    jsonFileFootStepsOfUser <- paste(a,trimws(b),sep="")
    JSONdata <- fromJSON(jsonFileFootStepsOfUser)
    
    output$text1 <- renderText({JSONdata[[1]]$airtime})
  })
  
})