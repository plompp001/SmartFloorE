library(jsonlite)
library(dplyr)

# Distance between positions in meters, distance between two positions equals 10cm.
distance_between_positions = 0.1
loadDataFromApi = FALSE

NEWsessionsDF = data.frame(Floor = character(),
                           Session = character(),
                           Footsteps = character())
NEWsessionsDF = readRDS(file = "NEWsessionsDF.Rda")

if (loadDataFromApi == TRUE) {
  NEWallSessions = fromJSON("http://track.smartfloor.com/api/v2/sessions")
  
  #doe voor elke sessie
  for (index in 1:nrow(NEWallSessions)) {
    NEWallSessionsId = NEWallSessions$id[[index]]
    
    if (!NEWallSessionsId %in% NEWsessionsDF$Session) {
      session_filename =
        paste("session_",
              NEWallSessionsId, "_*.Rda",
              sep = "")
      
      #doe als sessie al lokaal is opgeslagen
      if (file.exists(session_filename)) {
        totalFootsteps = substring(session_filename, 33)
        totalFootsteps = sub('.Rda', '', totalFootsteps, fixed = TRUE)
        
        #voeg de sessie toe aan het dataframe
        NEWsessionsDF =
          rbind(
            NEWsessionsDF,
            data.frame(
              Floor = NEWallSessions$floor$id[[index]],
              Session = NEWallSessionsId,
              Footsteps = totalFootsteps
            )
          )
      }
      
      #doe als sessie nog niet lokaal is opgeslagen
      if (!file.exists(session_filename)) {
        url =
          paste(
            "http://track.smartfloor.com/api/session/",
            NEWallSessionsId,
            "/stats",
            sep = ""
          )
        
        #probeer stats op te halen
        NEWsessieStats = try(fromJSON(url), silent = TRUE)
        
        #doe als er stats zijn
        if (class(NEWsessieStats) != "try-error") {
          if (length(NEWsessieStats) > 0) {
            #doe als de sessie minder dan 4000 voetstappen heeft
            if (as.integer(NEWsessieStats$totalFootsteps) < 4000) {
              url =
                paste(
                  "http://track.smartfloor.com/api/footstep/session/",
                  NEWallSessionsId,
                  sep = ""
                )
              
              session = try(fromJSON(url), silent = TRUE)
              
              #doe als er footsteps zijn
              if (class(session) != "try-error") {
                if (length(session) > 0) {
                  session_filename =
                    paste(
                      "session_",
                      NEWallSessionsId,
                      "_",
                      NEWsessieStats$totalFootsteps,
                      ".Rda",
                      sep = ""
                    )
                  
                  #sla voetstappen lokaal op
                  saveRDS(session, file = session_filename)
                  
                  #gooi de sessie ook nog in een dataframe
                  NEWsessionsDF =
                    rbind(
                      NEWsessionsDF,
                      data.frame(
                        Floor = NEWallSessions$floor$id[[index]],
                        Session = NEWallSessionsId,
                        Footsteps = NEWsessieStats$totalFootsteps
                      )
                    )
                }
              }
            }
          }
        }
      }
    }
  }
  saveRDS(NEWsessionsDF, file = "NEWsessionsDF.Rda")
}

all_floors_with_sessions_with_footsteps = NEWsessionsDF[NEWsessionsDF$Footsteps > 0, ]
all_floors_with_sessions_with_footsteps = arrange(all_floors_with_sessions_with_footsteps, Floor, Session)

floors =
  as.character(all_floors_with_sessions_with_footsteps$Floor)
sessionss =
  as.character(all_floors_with_sessions_with_footsteps$Session)

sessionsOfFirstFloor =
  filter(all_floors_with_sessions_with_footsteps, Floor == floors[[1]])
sessions = as.character(sessionsOfFirstFloor$Session)

sessieId = sessions[1]
nr = as.character(sessionsOfFirstFloor$Footsteps[[1]])



refresh <- function(sessieId) {
  sessieDF = filter(all_floors_with_sessions_with_footsteps,
                    Session == sessieId)
  
  nr = as.character(sessieDF$Footsteps[[1]])
  
  name = paste("session_", sessieId, "_", nr, ".Rda", sep = "")
  
  session = readRDS(file = name)
  
  players = data.frame(distinct(session$user))
  positions = data.frame(session$position, session$user)
  
  distance_per_player = c()
  average_speed_per_player = c()
  num_footsteps_per_player = c()
  player_name = "Unknown"
  for (index in 1:nrow(players)) {
    # Vector of all positions within a session for a particular player.
    positions_for_player = filter(positions, id == players[index, 1])
    
    # Distances between positions are calculated by adding up the x/y position differences.
    distances = positions_for_player %>%
      mutate(difference =
               abs(
                 positions_for_player$x - lag(positions_for_player$x, default = 0)
               ) +
               abs(
                 positions_for_player$y - lag(positions_for_player$y, default = 0)
               )) %>%
      .$difference
    
    # The total distance is calculated by multiplying the distance between tags with the sum of the distances between each step.
    distance_in_meters = distance_between_positions * sum(distances)
    
    # Total session time for a player.
    session_time = (session$time[nrow(session)] - session$time[1]) / 1000
    
    # Average speed in kilometers per hour for a particular player.
    speed_in_kph = (distance_in_meters / session_time) * 3.6
    
    player =
      fromJSON(paste0(
        "http://track.smartfloor.com/api/user/",
        toString(players$id)
      ))
    
    player_name = player$name
    
    # Concatenating calculated values to the vectors, which are declared above.
    distance_per_player = c(distance_per_player, distance_in_meters)
    average_speed_per_player =
      c(average_speed_per_player, speed_in_kph)
    num_footsteps_per_player =
      c(num_footsteps_per_player, nrow(positions_for_player))
  }
  
  # A data frame containing all relevant data per player(i.e. average speed, distance).
  player_data =
    data.frame(
      players$id,
      player_name,
      distance_per_player,
      average_speed_per_player,
      num_footsteps_per_player
    )
  player_data
  # Creation of a matrix for a floor, this is used to show a heatmap(or any sort of graph which utilizes the z-axis).
  floor = matrix(0, max(positions$x), max(positions$y))
  
  # For each step taken on a floor, the z value will be increased for a particular position to create a matrix.
  for (index in 1:nrow(positions)) {
    floor[positions[index, 1], positions[index, 2]] =
      floor[floor(positions[index, 1]), round(positions[index, 2])] + 1
  }
}

m <- refresh(sessieId)