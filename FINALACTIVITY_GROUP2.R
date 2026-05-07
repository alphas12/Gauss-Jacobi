library(shiny)

uses_gs <- list(
  list("Vestibulum feugiat dui quis diam convallis mattis."),
  list("Integer vehicula diam sed ligula ", span("highlighted text", class = "highlighted-text"), " commodo fringilla."),
  list("Lorem ipsum dolor sit amet, ", span("highlighted text", class = "highlighted-text"), " adipiscing elit."),
  list("Vestibulum feugiat dui quis diam convallis mattis."),
  list("Integer vehicula diam sed ligula ", span("highlighted text", class = "highlighted-text"), " commodo fringilla.")
)

uses_jacobi <- list(
  list("Vestibulum feugiat dui quis diam convallis mattis."),
  list("Integer vehicula diam sed ligula ", span("highlighted text", class = "highlighted-text"), " commodo fringilla."),
  list("Lorem ipsum dolor sit amet, ", span("highlighted text", class = "highlighted-text"), " adipiscing elit."),
  list("Vestibulum feugiat dui quis diam convallis mattis."),
  list("Integer vehicula diam sed ligula ", span("highlighted text", class = "highlighted-text"), " commodo fringilla.")
)

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
        text-align: justify;
        font-family: IBM-Plex-Sans;
        font-size: 20px;
        font-style: normal;
        font-weight: 400;
        line-height: normal;
        margin: 0;
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
        max-height: 100vh;
      }
  
      .landing-right{
        display: flex; 
        flex-direction: column;
        gap: 57px; 
        width: 30%;
      }
      
      /* GS Definitions Page */
      
      .definitions {
        display: flex;
        flex-direction: row;
        align-items: center;
        min-height: 100vh;  
      }
      
      .definitions-gs-left {
        display: flex;
        flex-direction: row;
        border-radius: 0 20px 20px 0;
        border: 3px solid #183D5E;
        border-left: none;
        background: #F6F7F8;
        box-shadow: 4px 4px 12.7px 0 rgba(0, 0, 0, 0.14);
        width: 73%;
        overflow: hidden;
      }
      
      .definitions-main-content {
        display: flex;
        flex-direction: column;
        padding: 60px 50px;
        gap: 22px;
      }
      
      .definitions-sub-content {
        display: flex;
        flex-direction: column;
        padding: 60px 50px;
        flex: 1;
        gap: 18px;
      }
      
      .definitions-sub-content-title {
        display:flex;
        flex-direction: column;
      }
      
      .definitions-sub-content-itemlist {
        display:flex;
        flex-direction: column; 
        gap: 16px;
      }
      
      .definitions-gs-sub-content-item {
        display: flex;
        border-right: 3px solid #164670;
        padding: 15px 12px;
        padding-left: 0;
        justify-content: right;      
      }
      
      .definitions-title {
        color: #183D5E;
        text-shadow: 0 4px 4px rgba(0, 0, 0, 0.09);
        font-family: ChunkFive;
        font-size: 40px;
        font-style: normal;
        font-weight: 400;
        line-height: normal;
      }
      
      /* Jacobi Definitions Page */
      
      .definitions-jacobi-right {
        display: flex;
        flex-direction: row;
        border-radius: 20px 0px 0px 20px;
        border: 3px solid #183D5E;
        border-right: none;
        background: #F6F7F8;
        box-shadow: 4px 4px 12.7px 0 rgba(0, 0, 0, 0.14);
        width: 73%;
        overflow: hidden;
      }
      
      .definitions-jacobi-sub-content-item {
        display: flex;
        border-left: 3px solid #164670;
        padding: 15px 12px;
        padding-right: 0;
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
      a("Definitions", href = "#definitions-gs", class = "nav-item"),
      a("Foundations", href = "#foundations", class = "nav-item"),
      a("Conditions", href = "#conditions", class = "nav-item"),
      a("Examples", href = "#examples", class = "nav-item"),
      a("Calculator", href = "#calculator", class = "nav-item")
  ),
  
  # Landing Page
  div(id = "landing", 
      
      img(src = "assets/landing-photo.png"),
      
      div(class="landing-right",
        img(src = "assets/title.svg", style = "display: block;"),
        p(style = "text-align: justify; margin: 0;", class = "body-text", 
          "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
          " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
          "highlighted-text"), " commodo fringilla."
        )
      )
      
  ),
  
  # Definitions Page
  
  # GS
  
  div(id = "definitions-gs", class ="definitions",
      
      div(class = "definitions-gs-left",
        div(style= "width:73%;", class="definitions-main-content",
          h1(class="definitions-title", "Gauss-Seidel Method"),
          p(class = "body-text", 
            "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
            " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                                                                  "highlighted-text"), " commodo fringilla."
          )
        ),
        div(style= "flex:1; background:#183D5E;", class="definitions-main-content",
          h1(style = "color:#F6F7F8;", class="definitions-title", "History"),
          p(style = "color: #F6F7F8;", class = "body-text", 
            "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
            " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                                                                  "highlighted-text"), " commodo fringilla."
          )
        )
      ),
      
      div(class = "definitions-sub-content",
          div(class="definitions-sub-content-title",
            img(src = "assets/quote.svg", style = "display: block; width:40px; height:auto"),
            h1(class="definitions-title", "When do we use it?")
          ),
          div(
            class = "definitions-sub-content-itemlist",
            lapply(uses_gs, function(item) {
              div(class = "definitions-gs-sub-content-item",
                p(style = "text-align: right;", class = "body-text", item)
              )
            })
          )
      )
  ),
  
  # Jacobi
  
  div(id ="definitions-jacobi", class ="definitions",
      
      div(class = "definitions-sub-content",
          div(style="align-items: end;", class="definitions-sub-content-title",
              img(src = "assets/quote.svg", style = "display: block; width:40px; height:auto"),
              h1(style="text-align: end;", class="definitions-title", "When do we use it?")
          ),
          div(
            class = "definitions-sub-content-itemlist",
            lapply(uses_jacobi, function(item) {
              div(class = "definitions-jacobi-sub-content-item",
                  p(class = "body-text", item)
              )
            })
          )
      ),
      
      div(class = "definitions-jacobi-right",
          div(style= "flex:1;", class="definitions-main-content",
              h1(class="definitions-title", "History"),
              p(class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                                                                  "highlighted-text"), " commodo fringilla."
              )
          ),
          div(style= "width:73%; background:#183D5E;", class="definitions-main-content",
              h1(style = "color: #F6F7F8", class="definitions-title", "Gauss-Seidel Method"),
              p(style = "color: #F6F7F8", class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
          fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
          consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                                                                  "highlighted-text"), " commodo fringilla."
              )
          )
      )
  ),
  
  # Placeholders
  div(id = "foundations", class = "section", h2("Foundations Section")),
  div(id = "conditions", class = "section", h2("Conditions Section")),
  div(id = "examples", class = "section", h2("Examples Section")),
  div(id = "calculator", class = "section", h2("Calculator Section"))
  
)

server <- function(input, output, session) {}

shinyApp(ui, server)