library(shiny)
library(RJSONIO)

# twee sessies met user met footsteps:
# 5891a3b92ab79c000531c42c
# 588a04cd2ab79c000531c18e

# haal sessie gegevens op:
jsonFileSession <- "https://tst-sport.trifork.nl/api/session/588a04cd2ab79c000531c18e"
JSONdataOfSession <- fromJSON(jsonFileSession)

# haal users van de sessie op:
jsonFileUsersFromSession <- "https://tst-sport.trifork.nl/api/session/588a04cd2ab79c000531c18e/user"
JSONdataUsersFromSession <- fromJSON(jsonFileUsersFromSession)

# maak vector list met alle spelers:
vector = c()
#vector <- c(vector, "Alle spelers")

for (i in 1:length(JSONdataUsersFromSession))
  x <- paste(JSONdataUsersFromSession[[i]]$name, " (", JSONdataUsersFromSession[[i]]$id,")")
  vector <- c(vector, paste(JSONdataUsersFromSession[[i]]$name, " (", JSONdataUsersFromSession[[i]]$id,")"))

  # pagina:
  shinyUI(fluidPage(
  titlePanel("SmartFloor Group E"),
  sidebarLayout(
    sidebarPanel((paste("Vloer:", JSONdataOfSession$floor$name)),
                 selectInput("dataset", "Kies een speler:",
                             choices = vector),
                 actionButton("button", "Toon stappen")),
    mainPanel("Airtime van stappen",
              tableOutput("table"))
  )
))