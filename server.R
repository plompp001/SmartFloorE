library(plotly)

source("api.R")
function(input, output) {
  
  output$amountOfFootsteps <- renderPlotly({
    plot_ly(player_data,
            x = players$id,
            y = num_footsteps_per_player,
            type = "bar")
  })
  
  output$averageSpeed <- renderPlotly({
    plot_ly(player_data,
            x = players$id,
            y = average_speed_per_player,
            type = "bar")
  })
  
  output$totalDistance <- renderPlotly({
    plot_ly(player_data,
            x = players$id,
            y = distance_per_player,
            type = "bar")
  })
  
  output$positions <- renderPlotly({
    plot_ly(positions,
            x = positions$x,
            y = positions$y,
            type = "scatter",
            mode = 'markers')
  })
  
  output$heatmap <- renderPlotly({
    plot_ly(x = max(positions$x) - 1.5, y = max(positions$y) - 1.5, z = field, type = "heatmap")
  })
  
  output$perspective <- renderPlot({
    persp(floor, expand = 0.2)
  })
  
  output$table <- renderDataTable({
    player_data
  })
}