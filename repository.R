library(mongolite)

connection <- mongo(collection = "footstep_mocap", db = "smartfloor", url = "mongodb://localhost",
      verbose = FALSE, options = ssl_options())

collection <- connection$find()

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
