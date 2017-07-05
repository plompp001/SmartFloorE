library(mongolite)

# Repository.R
#
# Script for querying data from a local mongoDB.

# Makes a connection with a mongo database instance at localhost.
# Uses the db: ‘smartfloor’ with the collection: ‘footstep_mocap’.
# Verbose is false to reduce the output in the console. No ssl is used default arguments are false. 
connection <- mongo(collection = "footstep_mocap", db = "smartfloor", url = "mongodb://localhost",
      verbose = FALSE, options = ssl_options())

# Query all data from the collection: ‘footstep_mocap’.
collection <- connection$find()

# Function that collects all unique sessions from the collection.
#
# return - data frame with all unique sessions.
findAllSessions <- function() {
  data.frame(distinct(collection$session))
}

# Function that queries a unique session from the database with the given id.
#
# return - data frame with 1 session.
findSessionById <- function(id) {
  data.frame(connection$find(paste0("{ \"session.$id\": \"", id, "\"}")))
}

# Function that collects all unique players from a session.
#
# return - data frame with all unique players within a session.
findPlayers <- function(id) {
  session <- findSessionById(id)
  data.frame(distinct(session$user))
}

# Function that finds all the positions within a session.
#
# return - data frame with all positions, which user it belongs to and a timestamp.
findPositions <- function(id) {
  session <- findSessionById(id)
  
  time = as.integer(as.POSIXct((session$time))) - as.integer(as.POSIXct((min(session$time))))

  data.frame(abs(session$position), session$user, time)
}

session_time_in_seconds = 52L
