library(jsonlite)
library(dplyr)
library(plotly)

session_data <- fromJSON("http://tst-sport.trifork.nl/api/footstep/session/588f20802ab79c000531c239")
users <- data.frame(distinct(session_data$user))
positions <- data.frame(session_data$position, session_data$user)

distance_pp <- c()
speed_pp <- c()

for(index in 1:nrow(users)) {
  user_positions <- filter(positions, id == users[index, 1])

  distances <- user_positions %>%
    mutate(difference = 
             abs(user_positions$x - lag(user_positions$x, default = 0)) +
             abs(user_positions$y - lag(user_positions$y, default = 0))) %>%
    .$difference
  
  distance_m = 0.1 * sum(distances)
  time_diff = (session_data$time[nrow(session_data)] - session_data$time[1]) / 1000 
  speed_kph = (distance_m / time_diff) * 3.6
  
  distance_pp <- c(distance_pp, distance_m)
  speed_pp <- c(speed_pp, speed_kph)
}

user_data <- data.frame(users$id, distance_pp, speed_pp)
user_data

field <- matrix(0, max(positions$x), max(positions$y))
field

for(index in 1:nrow(positions)) {
  field[positions[index, 1], positions[index, 2]] <- field[floor(positions[index, 1]), round(positions[index, 2])] + 1
}

plot_ly(x = max(positions$x) - 1.5, y = max(positions$y) - 1.5, z = field, type = "heatmap")
