library(shiny)
library(plotly)
library(shinydashboard)

source("api.R")


header <- dashboardHeader(title = "SMARTFLOOR E")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Footsteps", tabName = "tabFootsteps"),
  menuItem("Distance", tabName = "tabDistance"),
  menuItem("Speed", tabName = "tabSpeed"),
  menuItem("Positions", tabName = "tabPositions"),
  menuItem("Heatmap", tabName = "tabHeatmap"),
  menuItem("Perspective", tabName = "tabPerspective")
))

filters <- column(
  width = 3,
  box(
    width = NULL,
    status = "warning",
    selectInput("floors", "floor",
                choices = floors),
    actionButton("loadfloor", "Load floor"),
    p(
      class = "text-muted",
      br(),
      "Select a floor"
    )
  ),
  box(
    width = NULL,
    status = "warning",
    selectInput("sessions", "Session",
                choices = sessions),
    actionButton("loadSession", "Load session"),
    p(
      class = "text-muted",
      br(),
      "Currently all the sessions are from the floor with id: 'Eindhoven Test Vloer'."
    )
  ),
  box(
    width = NULL,
    status = "warning",
    selectInput("player1", "Player 1",
                choices = player_data$player_name),
    selectInput("player2", "Player 2",
                choices = player_data$player_name),
    actionButton("comparePlayers", "Compare Players"),
    p(
      class = "text-muted",
      br(),
      "Note: this will compare 2 players within the selected session. It can occur that a player didn't set a footstep."
    )
  ),
  box(
    width = NULL,
    status = "warning",
    sliderInput(
      "sessionTime",
      "Session flags",
      min = 0,
      max = session_time,
      value = c(0, session_time)
    ),
    p(class = "text-muted",
      paste("Note: time is in seconds")),
    actionButton("setSessionTime", "Set time"),
    br(),
    br(),
    sliderInput(
      "playSession",
      "Play session",
      min = 0,
      max = session_time,
      value = 0,
      step = 1,
      animate = animationOptions(interval =
                                   session_time, loop = TRUE)
    )
  )
)

body <- dashboardBody(
  fluidRow(column(width = 9,
  tabItems(
    tabItem(tabName = "tabFootsteps",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("amountOfFootsteps")
              ),
              box(width = NULL,
                  uiOutput("amountOfFootstepsTable"))
            ))),
    tabItem(tabName = "tabDistance",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("totalDistance")
              ),
              box(width = NULL,
                  uiOutput("totalDistanceTable"))
            ))),
    tabItem(tabName = "tabSpeed",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("averageSpeed")
              ),
              box(width = NULL,
                  uiOutput("averageSpeedTable"))
            ))),
    tabItem(tabName = "tabPositions",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("positions")
              )
            ))),
    tabItem(tabName = "tabHeatmap",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("heatmap")
              )
            ))),
    tabItem(tabName = "tabPerspective",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotOutput("perspective")
              )
            )))
  )
  ),column(
    width = 3,
    box(
      width = NULL,
      status = "warning",
      selectInput("floors", "floor",
                  choices = floors),
      actionButton("loadfloor", "Load floor"),
      p(
        class = "text-muted",
        br(),
        "Select a floor"
      )
    ),
    box(
      width = NULL,
      status = "warning",
      selectInput("sessions", "Session",
                  choices = sessions),
      actionButton("loadSession", "Load session"),
      p(
        class = "text-muted",
        br(),
        "Currently all the sessions are from the floor with id: 'Eindhoven Test Vloer'."
      )
    ),
    box(
      width = NULL,
      status = "warning",
      selectInput("player1", "Player 1",
                  choices = player_data$player_name),
      selectInput("player2", "Player 2",
                  choices = player_data$player_name),
      actionButton("comparePlayers", "Compare Players"),
      p(
        class = "text-muted",
        br(),
        "Note: this will compare 2 players within the selected session. It can occur that a player didn't set a footstep."
      )
    ),
    box(
      width = NULL,
      status = "warning",
      sliderInput(
        "sessionTime",
        "Session flags",
        min = 0,
        max = session_time,
        value = c(0, session_time)
      ),
      p(class = "text-muted",
        paste("Note: time is in seconds")),
      actionButton("setSessionTime", "Set time"),
      br(),
      br(),
      sliderInput(
        "playSession",
        "Play session",
        min = 0,
        max = session_time,
        value = 0,
        step = 1,
        animate = animationOptions(interval =
                                     session_time, loop = TRUE)
      )
    )
  ))
)

dashboardPage(header,
              sidebar,
              body)