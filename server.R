library(shiny)
library(RJSONIO)


shinyServer(function(input, output, session) {
  observeEvent(input$button, {
    
    a = "https://tst-sport.trifork.nl/api/footstep/session/588a04cd2ab79c000531c18e/user/"
    b <- sub("\\).*", "", sub(".*\\(", "", input$dataset))
    jsonFileFootStepsOfUser <- paste(a,trimws(b),sep="")
    JSONdata <- fromJSON(jsonFileFootStepsOfUser)
    
    df <- data.frame(tijd=character(),
                     airtime=character(),
                     voet=character(),
                     stringsAsFactors=FALSE)
    
    for (i in 1:length(JSONdata))
      df <- rbind(df, data.frame(time=JSONdata[[i]]$time, airtime=JSONdata[[i]]$airtime, voet=JSONdata[[i]]$side))
        output$table <- renderTable(df)
  })
})