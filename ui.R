library(shiny)
library(plotly)

# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("Smartfloor E"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("session", "Choose a Session:", players$id),
      selectInput("player", "Choose a Player:", players$id),
      
      sliderInput("sessionTime", 
                  "Session time", 
                  value = 500,
                  min = 0, 
                  max = 1000)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Amount of Footsteps", 
                           plotlyOutput("amountOfFootsteps")),
                  tabPanel("Average Speed", 
                           plotlyOutput("averageSpeed")),
                  tabPanel("Total Distance", 
                           plotlyOutput("totalDistance")),
                  tabPanel("Positions", 
                           plotlyOutput("positions")),
                  tabPanel("Heatmap", 
                           plotlyOutput("heatmap")),
                  tabPanel("Perspective", 
                           plotOutput("perspective")),
                  tabPanel("Table", 
                           dataTableOutput("table"))
      )
    )
  )
)