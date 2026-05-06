library(shiny)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Choose a number:", min = 1, max = 100, value = 50)
    ),
    mainPanel(
      textOutput("value")
    )
  )
)

server <- function(input, output, session) {
  output$value <- renderText({
    paste("You selected:", input$n)
  })
}

shinyApp(ui = ui, server = server)
