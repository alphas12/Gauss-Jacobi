library(shiny)

ui <- fluidPage(
  
  tags$head(
    
    # Google Fonts
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap",
      rel = "stylesheet"
    ),
    
    tags$style(HTML("

      html {
        scroll-behavior: smooth;
      }

      body {
        background: #F6F7F8;
      }

      /* NAVBAR */
      .custom-navbar {
        font-family: Inter, sans-serif;
        position: fixed;
        top: 65px;
        left: 50%;
        transform: translateX(-50%);
        width: 1314px;
        background: rgba(22, 70, 112, 0.80);
        padding: 20px 95px;
        border-radius: 100px;
        display: flex;
        justify-content: space-between;
        z-index: 1000;
        align-items: center;
        box-shadow: 0 8px 20px rgba(0,0,0,0.2);
      }

      .nav-item {
        color: #F6F7F8;
        text-align: center;
        font-family: Inter;
        font-size: 16px;
        font-style: normal;
        font-weight: 600;
        line-height: normal;
      }

      .nav-item:hover {
        color: #2EC4B6;
        text-align: center;
        font-family: Inter;
        font-size: 16px;
        font-style: normal;
        font-weight: 600;
        line-height: normal;
        text-decoration-line: underline;
        text-decoration-style: solid;
        text-decoration-skip-ink: auto;
        text-decoration-thickness: auto;
        text-underline-offset: auto;
        text-underline-position: from-font;
      }
      
      /* section to test navbar scrolling */
      .section {
        padding: 120px 0;
        margin-top: -120px;

    "))
  ),
  
  # Navbar
  div(class = "custom-navbar",
      a("Landing", href = "#landing", class = "nav-item"),
      a("Definitions", href = "#definitions", class = "nav-item"),
      a("Foundations", href = "#foundations", class = "nav-item"),
      a("Conditions", href = "#conditions", class = "nav-item"),
      a("Examples", href = "#examples", class = "nav-item"),
      a("Calculator", href = "#calculator", class = "nav-item")
  ),
  
  # Placeholders
  div(id = "landing", class = "section", h2("Landing Section")),
  div(id = "definitions", class = "section", h2("Definitions Section")),
  div(id = "foundations", class = "section", h2("Foundations Section")),
  div(id = "conditions", class = "section", h2("Conditions Section")),
  div(id = "examples", class = "section", h2("Examples Section")),
  div(id = "calculator", class = "section", h2("Calculator Section"))
  
)

server <- function(input, output, session) {}

shinyApp(ui, server)