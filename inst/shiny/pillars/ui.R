library(shinybusy)

ui <- fluidPage(
  add_busy_gif(
    src = "https://jeroen.github.io/images/banana.gif",
    height = 70, width = 70
  ),
  titlePanel("Eight pillars"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "symbol", label = "Enter symbol", "MSFT"),
      actionButton(inputId = "go", label = "Go!"),
      p(),
      textOutput("name"),
      textOutput("exchange"),
      p(),
      tableOutput("table")
    ),
    mainPanel(
      fluidRow(
        plotOutput("plot"),
      ),
      fluidRow(
        plotOutput("financials"),
      )
    )
  )
)
