library(mongolite)

connectionA <- mongo(collection = "footstep_mocap", db = "smartfloor", url = "mongodb://localhost",
      verbose = FALSE, options = ssl_options())

collection <- connectionA$find()

findAllSessions <- function() {
  data.frame(distinct(collectionA$session))
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
