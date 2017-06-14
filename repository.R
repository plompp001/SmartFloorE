library(mongolite)

connectionA <- mongo(collection = "footstep_mocap", db = "smartfloor", url = "mongodb://localhost",
      verbose = FALSE, options = ssl_options())

collection <- connectionA$find()

findAllSessions <- function() {
  data.frame(distinct(collection$session))
}

findSessionById <- function(id) {
  data.frame(connectionA$find(paste0("{ \"session.$id\": \"", id, "\"}")))
}

findPlayers <- function(id) {
  session <- findSessionById(id)
  data.frame(distinct(session$user))
}

findPositions <- function(id) {
  session <- findSessionById(id)
  data.frame(session$position, session$user)
}

findStepbySessionAndTime <- function(timeId, id) {
  data.frame(connectionA$find(paste0("{ \"time\": { \"$date\":  \"", timeId, "\"}, \"session.$id\": \"", id, "\"}")))
}

#De tijd uiteindelijk als getal van 4 cijfers weggewerkt. Dit in de toekomst nog op de een of andere manier mooi terugzetten naar een tijd voor in het filter
findTimesForFilterLow <- function(id) {
  timesDF <- data.frame(connectionA$find(paste0("{ \"session.$id\": \"", id, "\"}")))
  timesDF <- sort(timesDF$time)
  timesDF <- substr(timesDF, 15, 19)
  timesDF <- gsub(":", "", timesDF)
  timesDF <- as.numeric(timesDF)
  return(timesDF[1])
}

findTimesForFilterHigh <- function(id) {
  timesDF <- data.frame(connectionA$find(paste0("{ \"session.$id\": \"", id, "\"}")))
  timesDF <- sort(timesDF$time)
  timesDF <- substr(timesDF, 15, 19)
  timesDF <- gsub(":", "", timesDF)
  timesDF <- as.numeric(timesDF)
  return(tail(timesDF, n = 1))
}

#Default values 
session_time <- 145.653
lowestTimeForFilter <- 2239
highestTimeForFilter <- 2250
