library(mongolite)

connection <- mongo(collection = "footstep_mocap", db = "smartfloor", url = "mongodb://localhost",
      verbose = FALSE, options = ssl_options())

collection <- connection$find()

# Distance between positions in meters, distance between two positions equals 10cm.
distance_between_positions = 0.1

id <- "01"

findAllSessions <- function() {
  data.frame(distinct(collection$session))
}

findSessionById <- function(id) {
  data.frame(connection$find(paste0("{ \"session.$id\": \"", id, "\"}")))
}

findPlayers <- function(id) {
  session <- findSessionById(id)
  data.frame(distinct(session$user))
}

findPositions <- function(id) {
  session <- findSessionById(id)
  data.frame(session$position, session$user)
}

session <- findSessionById(id)



players <- findPlayers(id)
positions <- findPositions(id)

cmat <- cor(positions$x,positions$y)

print(positions)
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
  session_time = (session$time[nrow(session)] - session$time[1]) / 1000
  
  # Average speed in kilometers per hour for a particular player *as.Double() NOT SURE*.
  speed_in_kph = (distance_in_meters / as.double(session_time)) * 3.6
  
  # Concatenating calculated values to the vectors, which are declared above.
  distance_per_player <- c(distance_per_player, distance_in_meters)
  average_speed_per_player <- c(average_speed_per_player, speed_in_kph)
  num_footsteps_per_player <- c(num_footsteps_per_player, nrow(positions_for_player))
}

# A data frame containing all relevant data per player(i.e. average speed, distance).
player_data <- data.frame(players$X.id, num_footsteps_per_player, distance_per_player, average_speed_per_player)

# Creation of a matrix for a floor, this is used to show a heatmap(or any sort of graph which utilizes the z-axis).
floor <- matrix(max(positions$x), max(positions$y))
# 
# # For each step taken on a floor, the z value will be increased for a particular position to create a matrix.
#for(index in 1:nrow(positions)) {
#   floor[positions[index, 1], positions[index, 2]] <- floor[floor(positions[index, 1]), round(positions[index, 2])] + 1
#}

data <- list("player_data"=player_data,"positions"=positions,"floor"=floor)



