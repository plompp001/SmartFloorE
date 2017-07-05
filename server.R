library(plotly)
library(jsonlite)
library(mongolite)
library(dplyr)

source("repository.R")

# Server.R
#
# Script for processing data and binding it to the UI.

function(input, output) {
  
  # A reactive expression that uses input from the filters and returns a list of values. 
  # The reactive expression will update these values whenever a widget changes.
  data <- reactive({
    
    # Distance between positions in meters, distance between two positions on the actual floor equals 10cm.
    distance_between_positions = 0.1
    
    id <- input$sessions

    session <- findSessionById(id)
    players <- findPlayers(id)
    positions <- findPositions(id)
    
    positions = positions[positions[, "time"]>0, ]
    positions = positions[positions[, "time"]<input$playSession + 9, ]
    
    # Initialisation of vectors.
    distance_per_player <- c()
    average_speed_per_player <- c()
    num_footsteps_per_player <- c()

    player_name = "Unknown"
    
    for (index in 1:nrow(players)) {
      # Vector of all positions within a session for a particular player.
      positions_for_player <-
        filter(positions, X.id == players[index, 2])
      
      # Distances between positions are calculated by adding up the x/y position differences.
      distances <- positions_for_player %>%
        mutate(difference =
            abs(positions_for_player$x - lag(positions_for_player$x, default = 0)) +
            abs(positions_for_player$y - lag(positions_for_player$y, default = 0))) %>%
        .$difference
      
      # The total distance is calculated by multiplying the distance between tags with the sum of the distances between each step.
      distance_in_meters = distance_between_positions * sum(distances)
      
      # Total session time for a player.
      session_time_in_seconds = as.integer(as.POSIXct((max(session$time)))) - as.integer(as.POSIXct((min(session$time))))

      # Average speed in kilometers per hour for a particular player *as.Double() NOT SURE*.
      speed_in_kph = (abs(distance_in_meters) / as.double(session_time_in_seconds)) * 3.6
      
      # Concatenating calculated values to the vectors, which are declared above.
      distance_per_player <- c(distance_per_player, distance_in_meters)
      average_speed_per_player <- c(average_speed_per_player, speed_in_kph)
      num_footsteps_per_player <- c(num_footsteps_per_player, nrow(positions_for_player))
    }
    
    # A data frame containing all relevant data per player(i.e. average speed, distance).
    player_data <- data.frame(players$X.id, num_footsteps_per_player, distance_per_player, average_speed_per_player)
    
    min_x = (round(min(positions$x)) - 1)*-1
    min_y = (round(min(positions$y)) - 1)*-1
    max_x = round(max(positions$x)) + 1
    max_y = round(max(positions$y)) + 1

    # Creation of a matrix for a floor, this is used to show a heatmap(or any sort of graph which utilizes the z-axis).
    floor <- matrix(0, (max_y+min_y), (max_x+min_x))

    aantal = nrow(positions)
    
    for (index in 1:nrow(positions)) {
      x = round(positions$x[index])+min_x
      y = round(positions$y[index])+min_y
      if(!is.null(x) && !is.null(y) && x > 0 && y > 0){
         floor[positions[index, 1], positions[index, 2]] <- floor[floor(positions[index, 1]), round(positions[index, 2])] + 1
      }
    }
    
    data <- list("player_data"=player_data,"positions"=positions,"floor"=floor)
    
    na.omit(data)
  })
  
  output$amountOfFootsteps <- renderPlotly({
    data <- data()
    plot_ly(
      data$player_data,
      x = data$player_data$players.X.id,
      y = data$player_data$num_footsteps_per_player,
      type = "bar",
      color = ~ data$player_data$players.X.id
      
    ) %>%
      layout(title = 'Footsteps per Player',
             xaxis = list(title = 'Player',
                          zeroline = TRUE),
             yaxis = list(title = 'Total steps', list(type = "category")))
  })
 
  output$averageSpeed <- renderPlotly({
    data <- data()
    plot_ly(data$player_data,
            x = data$player_data$players.X.id,
            y = data$player_data$average_speed_per_player,
            type = "bar",
            color = ~ data$player_data$players.X.id)
    
  } %>%
    layout(title = 'Average speed',
           xaxis = list(title = 'Player',
                        zeroline = TRUE),
           yaxis = list(title = 'Average speed (in kilometers per hour)')))
    
  output$totalDistance <- renderPlotly({
    data <- data()
    plot_ly(data$player_data,
            x = data$player_data$players.X.id,
            y = data$player_data$distance_per_player,
            type = "bar",
            color = ~ data$player_data$players.X.id)
  }%>%
    layout(title = 'Total distance per player',
           xaxis = list(title = 'Player',
                        zeroline = TRUE),
           yaxis = list(title = 'Distance (in meters)')))
  
  output$positions <- renderPlotly({
    data <- data()
    plot_ly(
      data$positions,
      x = data$positions$x,
      y = data$positions$y,
      type = "scatter",
      mode = 'markers+lines',
      color = ~ data$positions$X.id
    )
  }
  %>%
    layout(title = 'Positions'))
  
  output$heatmap <- renderPlotly({
    data <- data()
    plot_ly(
      x = max(data$positions$x),
      y = max(data$positions$y),
      z = data$floor,
      type = "heatmap"
    )
  }
  %>%
    layout(title = 'Heatmap', xaxis = list(title = 'X-Axis'),
           yaxis = list(title = 'Y-Axis')))
  
  output$table <- renderDataTable({
    data()$player_data
  })
  
}
