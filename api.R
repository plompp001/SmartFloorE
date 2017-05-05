library(jsonlite)
library(dplyr)

# Distance between positions in meters, distance between two positions equals 10cm.
distance_between_positions = 0.1

session <- fromJSON("http://tst-sport.trifork.nl/api/footstep/session/588f20802ab79c000531c239")
players <- data.frame(distinct(session$user))
positions <- data.frame(session$position, session$user)

distance_per_player <- c()
average_speed_per_player <- c()
num_footsteps_per_player <- c()

for(index in 1:nrow(players)) {
  # Vector of all positions within a session for a particular player.
  positions_for_player <- filter(positions, id == players[index, 1])
  
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
  
  # Average speed in kilometers per hour for a particular player. 
  speed_in_kph = (distance_in_meters / session_time) * 3.6
  
  # Concatenating calculated values to the vectors, which are declared above.
  distance_per_player <- c(distance_per_player, distance_in_meters)
  average_speed_per_player <- c(average_speed_per_player, speed_in_kph)
  num_footsteps_per_player <- c(num_footsteps_per_player, nrow(positions_for_player))
}

# A data frame containing all relevant data per player(i.e. average speed, distance). 
player_data <- data.frame(players$id, distance_per_player, average_speed_per_player)

# Creation of a matrix for a floor, this is used to show a heatmap(or any sort of graph which utilizes the z-axis).
floor <- matrix(0, max(positions$x), max(positions$y))

# For each step taken on a floor, the z value will be increased for a particular position to create a matrix.
for(index in 1:nrow(positions)) {
  floor[positions[index, 1], positions[index, 2]] <- floor[floor(positions[index, 1]), round(positions[index, 2])] + 1
}
