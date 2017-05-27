library(plotly)

source("api.R")

shinyServer(function(input, output, session) {

  output$amountOfFootsteps <- renderPlotly({
    plot_ly(player_data,
            x = player_name,
            y = num_footsteps_per_player,
            type = "bar")
  })

  output$amountOfFootstepsTable <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("ID"),
                 tags$th("Name"),
                 tags$th("Number of Footsteps")
               )),
               tags$tbody(tags$tr(
                 tags$td(span(
                   style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     "#1F78B4"
                   )
                 )),
                 tags$td(player_data$players.id),
                 tags$td(player_data$player_name),
                 tags$td(num_footsteps_per_player)
               )))
  })
  
  output$averageSpeed <- renderPlotly({
    plot_ly(player_data,
            x = player_name,
            y = average_speed_per_player,
            type = "bar")
  })
  
  output$averageSpeedTable <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("ID"),
                 tags$th("Name"),
                 tags$th("Average Speed")
               )),
               tags$tbody(tags$tr(
                 tags$td(span(
                   style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     "#1F78B4"
                   )
                 )),
                 tags$td(player_data$players.id),
                 tags$td(player_data$player_name),
                 tags$td(player_data$average_speed_per_player)
               )))
  })
  
  output$totalDistance <- renderPlotly({
    plot_ly(player_data,
            x = player_name,
            y = distance_per_player,
            type = "bar")
  })
  
  output$totalDistanceTable <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("ID"),
                 tags$th("Name"),
                 tags$th("Total distance")
               )),
               tags$tbody(tags$tr(
                 tags$td(span(
                   style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     "#1F78B4"
                   )
                 )),
                 tags$td(player_data$players.id),
                 tags$td(player_data$player_name),
                 tags$td(player_data$distance_per_player)
               )))
  })
  
  output$positions <- renderPlotly({
    plot_ly(
      positions,
      x = positions$x,
      y = positions$y,
      type = "scatter",
      mode = 'markers'
    )
  })
  
  output$heatmap <- renderPlotly({
    plot_ly(
      x = max(positions$x) - 1.5,
      y = max(positions$y) - 1.5,
      z = floor,
      type = "heatmap"
    )
  })
  
  output$perspective <- renderPlot({
    persp(floor, expand = 0.2)
  })
  
  output$table <- renderDataTable({
    player_data
  })
  
  observeEvent(input$loadfloor, {
    
    sessionOfFloor <- filter(all_floors_with_sessions_with_footsteps, Floor == input$floors)
    sessions <- as.character(sessionsOfFirstFloor$Session)
    
    updateSelectInput(session, "sessions", choices =  sessions)
  })
  
  observeEvent(input$loadSession, {
    
    #url <- paste("http://tst-sport.trifork.nl/api/session/",
    #            input$sessions,"/user", sep = "");
    #session  <- fromJSON(url);
    
    m<-myFirstFun(input$sessions)
    
    
    positions <- data.frame(session$position, session$user)
    
    
    output$plots=renderUI({plotOutput("positions")})
    output$positions <- renderPlotly({
      plot_ly(
        positions,
        x = positions$x,
        y = positions$y,
        type = "scatter",
        mode = 'markers'
      )
    })
  })

})