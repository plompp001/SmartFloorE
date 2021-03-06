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
  menuItem("Table", tabName = "tabTable")
)) 

filters <- column(
  width = 3,
  box(
    width = NULL,
    status = "success",
    selectInput("sessions", "Session",
                choices = sort(findAllSessions()$X.id))
  ),
  box(
    width = NULL,
    status = "success",
    sliderInput(
      "playSession",
      "Replay session (time in seconds)",
      min = 0,
      max = session_time_in_seconds,
      value = 0,
      step = 1,
      animate = animationOptions(interval = 1000 , loop = FALSE)
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
    tabItem(tabName = "tabTable",
            fluidRow(column(
              width = 12,
              box(
                width = NULL,
                solidHeader = TRUE,
                dataTableOutput("table")
              )
            )))
  )
), filters))

dashboardPage(skin = "green",
              header,
              sidebar,
              body)