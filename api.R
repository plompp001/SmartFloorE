library(jsonlite)
library(dplyr)

# Distance between positions in meters, distance between two positions equals 10cm.
distance_between_positions = 0.1

loadFromApi <- FALSE


testpppp <- ""
testpppppp <- ""
sessionsVector <- c()

usersfromsession = ""

## overbodige code door nieuwe api-url
if (loadFromApi == TRUE) {
  floorsFromApi <- fromJSON("http://tst-sport.trifork.nl/api/floors")
  saveRDS(floorsFromApi, file = "floors.Rda")
  
  
  floors <- readRDS(file = "floors.Rda")
  sessionsList <- c()
  
  slist <- list()
  
  for (index in 1:nrow(floors)) {
    floorId <- floors$id[[index]]
    
    
    if (floorId != "") {
      url <- paste(
        "http://tst-sport.trifork.nl/api/floors/",
        gsub(" ", "%20", trimws(floors$id[[index]])),
        "/sessions",
        sep = ""
      )
      
      sessionsFromFloor <- fromJSON(url)
      
      saveRDS(sessionsFromFloor,
              file = paste("sessions_of_floor_", floorId, ".Rda", sep = ""))
      
    }
  }
  url <-
    "http://tst-sport.trifork.nl/api/footstep/session/588f20802ab79c000531c239"
  
  
  firstsessionJson <- fromJSON(url)
  
  saveRDS(firstsessionJson, file = "firstSession.Rda")
  
}



floors <- readRDS(file = "floors.Rda")
floor_with_footsteps = ""

session_with_footsteps = ""

sessionsVector = c()


sessions <- readRDS(file = "sessions_of_floor_10.Rda")
all_session_ids = list()
all_session_floor_ids = list()
df <- data.frame(Floor = character(),
                 Session = character(),
                 User = character())

##hiermee worden alle sessies met voetstappen in een dataframe gestopt
if (loadFromApi == TRUE) {
  for (index in 1:nrow(floors)) {
    floorId <- floors$id[[index]]
    
    if (floorId != "") {
      session = readRDS(file = paste("sessions_of_floor_", floorId, ".Rda", sep = ""))
      
      if (!length(session) == 0) {
        for (index in 1:nrow(session)) {
          url_footsteps_of_single_session <-
            paste("http://track.smartfloor.com/api/footstep/session/",
                  session$id[[index]],
                  sep = "")
          
          footsteps_of_single_session = tryCatch(
            fromJSON(url_footsteps_of_single_session),
            error = function(e)
              NULL
          )
          
          sessieId = session$id[[index]]
          
          if (!is.null(footsteps_of_single_session)) {
            ppp = footsteps_of_single_session
          }
          if (length(ppp) > 0) {
            for (index in 1:nrow(ppp)) {
              df <-
                rbind(df,
                      data.frame(
                        Floor = floorId,
                        Session = sessieId,
                        User = ppp$user$id[[index]]
                      ))
            }
          }
        }
      }
    }
  }
  saveRDS(df, file = "all_floors_with_sessions_with_footsteps.Rda")
}

all_floors_with_sessions_with_footsteps = readRDS(file = "all_floors_with_sessions_with_footsteps.Rda")

dfshowinfo = data.frame(all_session_ids, all_session_floor_ids)

myFirstFun <- function(sessieId)
{
  url <-
    paste("http://track.smartfloor.com/api/footstep/session/",
          sessieId,
          sep = "")
  
  session <- fromJSON(url)
  players <- data.frame(distinct(session$user))
  positions <- data.frame(session$position, session$user)
  
  distance_per_player <- c()
  average_speed_per_player <- c()
  num_footsteps_per_player <- c()
  player_name = "Unknown"
  for (index in 1:nrow(players)) {
    # Vector of all positions within a session for a particular player.
    positions_for_player <- filter(positions, id == players[index, 1])
    
    # Distances between positions are calculated by adding up the x/y position differences.
    distances <- positions_for_player %>%
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
    
    player <-
      fromJSON(paste0(
        "http://track.smartfloor.com/api/user/",
        toString(players$id)
      ))
    
    #player = readRDS(file="firstPlayer");
    player_name = player$name
    
    # Concatenating calculated values to the vectors, which are declared above.
    distance_per_player <- c(distance_per_player, distance_in_meters)
    average_speed_per_player <-
      c(average_speed_per_player, speed_in_kph)
    num_footsteps_per_player <-
      c(num_footsteps_per_player, nrow(positions_for_player))
  }
  
  # A data frame containing all relevant data per player(i.e. average speed, distance).
  player_data <-
    data.frame(
      players$id,
      player_name,
      distance_per_player,
      average_speed_per_player,
      num_footsteps_per_player
    )
  player_data
  # Creation of a matrix for a floor, this is used to show a heatmap(or any sort of graph which utilizes the z-axis).
  floor <- matrix(0, max(positions$x), max(positions$y))
  
  # For each step taken on a floor, the z value will be increased for a particular position to create a matrix.
  for (index in 1:nrow(positions)) {
    floor[positions[index, 1], positions[index, 2]] <-
      floor[floor(positions[index, 1]), round(positions[index, 2])] + 1
  }
}

sessionss <-
  as.character(all_floors_with_sessions_with_footsteps$Session)
firstSessionId = sessionss[[1]]

m <- myFirstFun(firstSessionId)