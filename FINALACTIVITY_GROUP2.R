library(shiny)

ui <- fluidPage(
  
  tags$head(
    
    tags$link(
    ),
    
    tags$style(HTML("
    
      @font-face {
        font-family: 'Chunk-Five';
        src: url('fonts/chunkfive.regular.ttf') format('truetype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'Inter';
        src: url('fonts/Inter-VariableFont_opsz,wght.ttf') format('truetype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'IBM-Plex-Sans';
        src: url('fonts/IBMPlexSans-VariableFont_wdth,wght.ttf') format('truetype');
        font-weight: normal;
        font-style: normal;
      }
      
      .highlighted-text {
        color: #FF576F;
        font-family: IBM-Plex-Sans;
        font-size: 20px;
        font-style: normal;
        font-weight: 600;
        line-height: normal;
      }
      
      .body-text {
        color: #164670;
        text-align: center;
        font-family: IBM-Plex-Sans;
        font-size: 20px;
        font-style: normal;
        font-weight: 400;
        line-height: normal;
      }
      

      html {
        scroll-behavior: smooth;
      }

      body {
        background: #F6F7F8;
      }

      /* Navbar */
      
      .custom-navbar {
        font-family: Inter, sans-serif;
        position: fixed;
        top: 65px;
        left: 0;
        right: 0;
        width: calc(100% - 126px); 
        margin: 0 auto; 
        background: rgba(22, 70, 112, 0.80);
        padding: 20px 95px;
        border-radius: 100px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        box-shadow: 0 8px 20px rgba(0,0,0,0.2);
        z-index: 1000;
      }
      
      .nav-item {
        color: #F6F7F8;
        text-align: center;
        font-family: Inter;
        font-size: 16px;
        font-style: normal;
        font-weight: 600;
        line-height: normal;
        text-shadow: 0 4px 4px 0 rgba(0, 0, 0, 0.25);
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
        text-shadow: 0 4px 4px 0 rgba(0, 0, 0, 0.25);
      }
      
      /* Landing Page */
      
      #landing {
        display: flex;
        justify-content: space-evenly;
        flex-direction: row;
        align-items: center;
        min-height: 100vh;  
      }
  
      .landing-right{
        display: flex; 
        flex-direction: column;
        gap: 57px; 
        width: 30%;
      }
      
      .landing-title {
        style = display: block;
      }
      
      /* For Placeholders :) */
      
      .section {
        margin: 20% 0;
      }

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
  
  # Landing Page
  div(id = "landing", 
      
      img(src = "assets/landing-photo.png"),
      
      div(class="landing-right",
        img(src = "assets/title.svg", class="landing-title"),
        p(style = "text-align: justify;", class = "body-text", 
          "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
          " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
          "highlighted-text"), " commodo fringilla.
        ")
      )
      
  ),
  
  # Placeholders
  div(id = "definitions", class = "section", h2("Definitions Section")),
  div(id = "foundations", class = "section", h2("Foundations Section")),
  div(id = "conditions", class = "section", h2("Conditions Section")),
  div(id = "examples", class = "section", h2("Examples Section")),
  div(id = "calculator", class = "section", h2("Calculator Section"))
  
)

server <- function(input, output, session) {}

shinyApp(ui, server)