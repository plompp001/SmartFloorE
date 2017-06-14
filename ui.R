library(shiny)
library(plotly)
library(shinydashboard)

source("repository.R")

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
    selectInput("sessions", "Session",
                choices = sort(findAllSessions()$X.id),
                selected = TRUE)
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
      min = as.numeric(lowestTimeForFilter), 
      max = as.numeric(highestTimeForFilter), 
      value = 0,
      step = 1,
      animate = animationOptions(interval =
                                   session_time, loop = TRUE)
    )
  )
)

body <- dashboardBody(fluidRow(column(
  width = 9,
  tabItems(
    tabItem(tabName = "tabFootsteps",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("amountOfFootsteps")
              )
            ))),
    tabItem(tabName = "tabDistance",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("totalDistance")
              )
            ))),
    tabItem(tabName = "tabSpeed",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                plotlyOutput("averageSpeed")
              )
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
), filters))

dashboardPage(header,
              sidebar,
              body)