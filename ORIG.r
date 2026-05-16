library(shiny)

# Iterative solver helpers (safe, general-purpose implementations)
jacobi_iterations <- function(A, b, x0, n_iter) {
  A <- as.matrix(A)
  n <- length(b)
  if (nrow(A) != n || ncol(A) != n) stop("A must be an n x n matrix and length(b) must be n")
  history <- matrix(0, nrow = n_iter + 1, ncol = n)
  history[1, ] <- as.numeric(x0)
  errors <- numeric(n_iter + 1)
  errors[1] <- NA
  x_prev <- as.numeric(x0)
  for (k in seq_len(n_iter)) {
    x_new <- numeric(n)
    for (i in seq_len(n)) {
      if (n == 1) {
        x_new[i] <- b[i] / A[i, i]
      } else {
        sum_others <- sum(A[i, -i] * x_prev[-i])
        x_new[i] <- (b[i] - sum_others) / A[i, i]
      }
    }
    history[k + 1, ] <- x_new
    errors[k + 1] <- sqrt(sum((x_new - x_prev)^2))
    x_prev <- x_new
  }
  colnames(history) <- paste0("x", seq_len(n))
  list(history = as.data.frame(history), errors = errors)
}

gauss_seidel_iterations <- function(A, b, x0, n_iter) {
  A <- as.matrix(A)
  n <- length(b)
  if (nrow(A) != n || ncol(A) != n) stop("A must be an n x n matrix and length(b) must be n")
  history <- matrix(0, nrow = n_iter + 1, ncol = n)
  x <- as.numeric(x0)
  history[1, ] <- x
  errors <- numeric(n_iter + 1)
  errors[1] <- NA
  for (k in seq_len(n_iter)) {
    x_old <- x
    for (i in seq_len(n)) {
      if (n == 1) {
        x[i] <- b[i] / A[i, i]
      } else {
        sum_others <- sum(A[i, -i] * x[-i])
        x[i] <- (b[i] - sum_others) / A[i, i]
      }
    }
    history[k + 1, ] <- x
    errors[k + 1] <- sqrt(sum((x - x_old)^2))
  }
  colnames(history) <- paste0("x", seq_len(n))
  list(history = as.data.frame(history), errors = errors)
}

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
    
    tags$style(HTML("
    
      @// THE STYLES ARE HERE
    
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
  
  div(id ="definitions-jacobi", style="padding: 0;", class ="definitions",
      
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
  
  # Foundations Page
  div(id = "foundations",
      div(class = "foundations-content-main", 
          div(class = "foundations-content", 
              h1(class="foundations-title", "Mathematical Foundations")
          ),
          div(style= "border-top: 4px solid #F6F7F8;", class = "foundations-content",
              p(style = "color: #F6F7F8", class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
                fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
                consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class =
                "highlighted-text"), " commodo fringilla.")
          )
      ),
          
  ),
  
  # Conditions Page
  div(id = "conditions",
      div(class="conditions-content", 
          div(class="conditions-subcontent", 
              div(class="conditions-title ", "Conditions"),
              p(class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
                fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
                consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                "highlighted-text"), " commodo fringilla.")
          ),
          div(class="conditions-subcontent", 
              div(class="conditions-subtitle ", "Convergence"),
              p(class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
                fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
                consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                "highlighted-text"), " commodo fringilla.")
          ),
          div(class="conditions-subcontent", 
              div(class="conditions-subtitle ", "Divergence"),
              p(class = "body-text", 
                "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
                " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
                fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
                consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
                "highlighted-text"), " commodo fringilla.")
          ),
      ),
      div(style = "background: #183D5E; border-radius: 20px; margin: 60px 50px; margin-left: 0;", class="conditions-content",
          div(style="color: #F6F7F8;", class="conditions-subtitle ", "Convergence"),
          p(style="color: #F6F7F8;", class = "body-text", 
            "Lorem ipsum dolor sit amet ", span("highlighted text", class = "highlighted-text"), 
            " adipiscing elit. Ut maximus commodo purus. Nulla eget ligula in tortori aculis 
            fringilla. Vestibulum feugiat dui quis diam convallis mattis. Praesent blandit convallis 
            consectetur. Integer vehicula diam sed ligula ", span("highlighted text", class = 
            "highlighted-text"), " commodo fringilla.")
      )
  ),
  
  # Examples Section
  div(id = "examples", class = "section",
    div(class = "example-section-background",
      div(class = "example-section-box"),
      div(class = "example-section-inner",
          div(class = "nav-frame",
              # Left: Method buttons
              div(class = "nav-buttons",
                  tags$button(
                    type = "button",
                    class = "method-btn",
                    onclick = "Shiny.setInputValue('example_method', 'gauss', {priority: 'event'})",
                    "Gauss-Seidel"
                  ),
                  tags$button(
                    type = "button",
                    class = "method-btn",
                    onclick = "Shiny.setInputValue('example_method', 'jacobi', {priority: 'event'})",
                    "Jacobi"
                  )
              ),
              
              # Right: Page counter and arrows
              div(class = "nav-controls",
                  # Left Arrow (conditional)
                  uiOutput("leftArrowUI"),
              
                  # Page Counter
                  div(class = "page-counter", textOutput("exampleCounter", inline = TRUE)),
                  
                  # Right Arrow (conditional)
                  uiOutput("rightArrowUI")
              )
          ),
          
          # Example Problems Title
          h2(class = "example-title", "Example Problems"),
          
          div(style = "display: flex; flex-direction: column",
              
              # Step-by-step Algorithm
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'algo')",
                      div(class = "title-text", "Step-by-step Algorithm"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "algo", class = "collapse-content", uiOutput("algoContent"))
              ),
              
              # Given System
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'system')",
                      div(class = "title-text", "Given System"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "system", class = "collapse-content", uiOutput("systemContent"))
              ),
              
              # Initial Guess
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'guess')",
                      div(class = "title-text", "Initial Guess"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "guess", class = "collapse-content", uiOutput("guessContent"))
              ),
              
              # Iterations
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'iter')",
                      div(class = "title-text", "Iterations"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "iter", class = "collapse-content", uiOutput("iterContent"))
              ),
              
              # Convergence
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'conv')",
                      div(class = "title-text", "Convergence"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "conv", class = "collapse-content", uiOutput("convContent"))
              ),
              
              # Error
              div(style = "display: flex; flex-direction: column",
                  div(class = "collapse-title", onclick = "toggleCollapse(this, 'error')",
                      div(class = "title-text", "Error"),
                      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
                  ),
                  div(id = "error", class = "collapse-content", uiOutput("errorContent"))
              )
          )
      ),
      div(class = "example-section-box")
    )
  ),

  div(id = "calculator", class = "section",
    div(class = "calculator-shell",
        div(class = "calculator-panel",
            div(class = "calculator-panel-header",
                h2(class = "panel-title", "Calculator")
            ),
            div(class = "calculator-panel-body",
                uiOutput("calculatorMethodButtons"),
                div(class = "calc-field-group",
                    span(class = "calc-box-label", "A ="),
                    div(class = "calc-textarea",
                        textAreaInput(
                          "calc_matrixA",
                          label = NULL,
                          placeholder = "Enter square matrix values\nExample:\n4 1 2; 1 5 1; 2 1 6",
                          rows = 5
                        )
                    )
                ),
                div(class = "calc-field-group",
                    span(class = "calc-box-label", "b ="),
                    div(class = "calc-textarea",
                        textAreaInput(
                          "calc_vectorb",
                          label = NULL,
                          placeholder = "Enter vector values\nExample:\n7, 8, 9",
                          rows = 3
                        )
                    )
                ),
                div(class = "calc-field-group",
                    div(class = "calc-inline-row",
                        span(class = "calc-inline-label", "Tolerance ="),
                        div(class = "calc-mini-input",
                            numericInput(
                              "calc_tolerance",
                              label = NULL,
                              value = 0.001,
                              min = 0,
                              step = 0.001
                            )
                        )
                    )
                ),
                div(class = "calc-field-group",
                    div(class = "calc-inline-row",
                        span(class = "calc-inline-label", "Max Iterations ="),
                        div(class = "calc-mini-input",
                            numericInput(
                              "calc_max_iter",
                              label = NULL,
                              value = 500,
                              min = 1,
                              step = 1
                            )
                        )
                    )
                ),
                div(class = "calc-field-group",
                    div(class = "calc-inline-row",
                        span(class = "calc-inline-label", "Initial Guess ="),
                        div(class = "calc-text-input",
                            textInput(
                              "calc_initial_guess",
                              label = NULL,
                              placeholder = "Optional (default is all 0's)\nExample: 0, 0, 0"
                            )
                        )
                    )
                ),
                actionButton(
                  "calc_submit",
                  "Enter",
                  class = "calc-submit-btn"
                ),
                div(
                  class = "calc-help-text",
                  "Input a square matrix A, vector b, tolerance, initial guess, and maximum iterations before clicking Enter."
                )
            )
        ),
        div(class = "solutions-panel",
            div(class = "solutions-panel-header",
                h2(class = "solutions-title", "Solutions"),
                uiOutput("calculatorTabs")
            ),
            div(class = "solutions-panel-body",
                uiOutput("calculatorSolutionUI")
            )
        )
    )
  ),
  
  tags$script(HTML("
    function toggleCollapse(element, id) {
      const content = document.getElementById(id);
      const isOpen = content.classList.toggle('show');
      if (isOpen) {
        element.classList.add('open');
        element.setAttribute('aria-expanded', 'true');
      } else {
        element.classList.remove('open');
        element.setAttribute('aria-expanded', 'false');
      }
    }

    Shiny.addCustomMessageHandler('resetExampleCollapses', function() {
      ['algo', 'system', 'guess', 'iter', 'conv', 'error'].forEach(function(id) {
        const content = document.getElementById(id);
        const trigger = content ? content.previousElementSibling : null;
        if (content) {
          content.classList.remove('show');
        }
        if (trigger) {
          trigger.classList.remove('open');
          trigger.setAttribute('aria-expanded', 'false');
        }
      });
    });
  "))
)



server <- function(input, output, session) {
  example_bank <- list(
    gauss = list(
      list(
        algo = "<h5>Gauss-Seidel Iteration Formula:</h5><p><code>x<sub>i</sub><sup>(k+1)</sup> = (b<sub>i</sub> - Σ<sub>j&lt;i</sub> a<sub>ij</sub> x<sub>j</sub><sup>(k+1)</sup> - Σ<sub>j&gt;i</sub> a<sub>ij</sub> x<sub>j</sub><sup>(k)</sup>) / a<sub>ii</sub></code></p><p>Use the newest available values as each variable is updated.</p>",
        system = c("3x₁ + x₂ = 5", "x₁ + 4x₂ = 6"),
        A = matrix(c(3, 1, 1, 4), nrow = 2, byrow = TRUE),
        b = c(5, 6),
        guess = c(0, 0),
        iter = 4,
        conv = "The values stabilize quickly because the matrix is diagonally dominant.",
        err = "The approximation error decreases as each sweep updates x₁ and x₂.",
        series = list(x1 = c(0, 1.67, 1.11, 1.30, 1.23), x2 = c(0, 1.50, 1.22, 1.19, 1.20))
      ),
      list(
        algo = "<h5>Update Strategy:</h5><p>Gauss-Seidel reuses the latest values immediately during the same iteration.</p>",
        system = c("4x₁ - x₂ = 3", "2x₁ + 5x₂ = 7"),
        A = matrix(c(4, -1, 2, 5), nrow = 2, byrow = TRUE),
        b = c(3, 7),
        guess = c(1, 1),
        iter = 5,
        conv = "Watch how the iterates move toward the solution with fewer oscillations.",
        err = "Each step lowers the residual more aggressively than the previous one.",
        series = list(x1 = c(1, 1.00, 1.33, 1.40, 1.42, 1.43), x2 = c(1, 0.60, 0.67, 0.65, 0.64, 0.64))
      ),
      list(
        algo = "<h5>Tip:</h5><p>Choose a good initial guess when the system is close to singular or mildly coupled.</p>",
        system = c("10x₁ + 2x₂ = 14", "x₁ + 7x₂ = 8"),
        A = matrix(c(10, 2, 1, 7), nrow = 2, byrow = TRUE),
        b = c(14, 8),
        guess = c(0, 0),
        iter = 4,
        conv = "The stronger diagonal dominance makes convergence very visible.",
        err = "The error curve should drop steadily after the first two iterations.",
        series = list(x1 = c(0, 1.40, 1.12, 1.14, 1.13), x2 = c(0, 1.14, 1.12, 1.12, 1.12))
      ),
      list(
        algo = "<h5>Practice Case:</h5><p>This example is useful for comparing convergence speed against Jacobi.</p>",
        system = c("8x₁ - x₂ = 7", "3x₁ + 9x₂ = 24"),
        A = matrix(c(8, -1, 3, 9), nrow = 2, byrow = TRUE),
        b = c(7, 24),
        guess = c(0, 0),
        iter = 6,
        conv = "The sequence approaches the fixed point through successive substitutions.",
        err = "Use the table to compare the change between successive iterates.",
        series = list(x1 = c(0, 0.88, 0.98, 0.99, 1.00, 1.00, 1.00), x2 = c(0, 2.37, 2.34, 2.34, 2.33, 2.33, 2.33))
      )
    ),
    jacobi = list(
      list(
        algo = "<h5>Jacobi Iteration Formula:</h5><p><code>x<sub>i</sub><sup>(k+1)</sup> = (b<sub>i</sub> - Σ<sub>j≠i</sub> a<sub>ij</sub> x<sub>j</sub><sup>(k)</sup>) / a<sub>ii</sub></code></p><p>All variables are updated using values from the previous iteration only.</p>",
        system = c("4x₁ + x₂ = 1", "2x₁ + 3x₂ = 2"),
        A = matrix(c(4, 1, 2, 3), nrow = 2, byrow = TRUE),
        b = c(1, 2),
        guess = c(0, 0),
        iter = 4,
        conv = "The method converges more slowly because each update waits for the full previous vector.",
        err = "The error is measured using the distance between successive iteration vectors.",
        series = list(x1 = c(0, 0.25, 0.083, 0.104, 0.099), x2 = c(0, 0.667, 0.500, 0.528, 0.521))
      ),
      list(
        algo = "<h5>Key Difference:</h5><p>Jacobi uses the full previous vector for every update, making it easy to parallelize.</p>",
        system = c("3x₁ - x₂ = 4", "x₁ + 5x₂ = 12"),
        A = matrix(c(3, -1, 1, 5), nrow = 2, byrow = TRUE),
        b = c(4, 12),
        guess = c(1, 1),
        iter = 5,
        conv = "The path is smoother but typically slower than Gauss-Seidel.",
        err = "Residuals shrink gradually as the updates repeat.",
        series = list(x1 = c(1, 1.67, 1.78, 1.81, 1.82, 1.82), x2 = c(1, 2.20, 2.07, 2.04, 2.03, 2.03))
      ),
      list(
        algo = "<h5>Reminder:</h5><p>Jacobi is often used as a benchmark when studying iterative solvers.</p>",
        system = c("6x₁ + x₂ = 7", "x₁ + 4x₂ = 8"),
        A = matrix(c(6, 1, 1, 4), nrow = 2, byrow = TRUE),
        b = c(7, 8),
        guess = c(0, 0),
        iter = 4,
        conv = "The sequence updates both variables simultaneously at each iteration.",
        err = "The approximation error is visible in the table of iterates.",
        series = list(x1 = c(0, 1.17, 0.97, 1.00, 0.99), x2 = c(0, 2.00, 1.71, 1.76, 1.75))
      ),
      list(
        algo = "<h5>Practice Case:</h5><p>Use this example to compare the convergence pattern against Gauss-Seidel.</p>",
        system = c("5x₁ - 2x₂ = 4", "2x₁ + 6x₂ = 18"),
        A = matrix(c(5, -2, 2, 6), nrow = 2, byrow = TRUE),
        b = c(4, 18),
        guess = c(0, 0),
        iter = 6,
        conv = "The iterates slowly approach the solution because of the full-step update style.",
        err = "The error table makes it easy to track the change from one iteration to the next.",
        series = list(x1 = c(0, 0.80, 1.12, 1.25, 1.30, 1.32, 1.33), x2 = c(0, 3.00, 2.73, 2.59, 2.53, 2.51, 2.50))
      )
    )
    
  )

  state <- reactiveValues(method = "gauss", page = 1, calcMethod = "gauss", calculatorTab = "lud")

  
  observeEvent(input$example_method, {
    state$method <- input$example_method
    state$page <- 1
    session$sendCustomMessage("resetExampleCollapses", list())
  }, ignoreInit = TRUE)

  observeEvent(input$page_prev, {
    state$page <- if (state$page <= 1) 4 else state$page - 1
    session$sendCustomMessage("resetExampleCollapses", list())
  }, ignoreInit = TRUE)

  observeEvent(input$page_next, {
    state$page <- if (state$page >= 4) 1 else state$page + 1
    session$sendCustomMessage("resetExampleCollapses", list())
  }, ignoreInit = TRUE)

  observeEvent(input$calc_method, {
    state$calcMethod <- input$calc_method
  }, ignoreInit = TRUE)

  observeEvent(input$calculatorTab, {
    state$calculatorTab <- input$calculatorTab
  }, ignoreInit = TRUE)

  observeEvent(input$calc_submit, {
    

    state$calculatorTab <- "solutions"
    
    # INIT ERROR STORAGE
    errors <- list()
    
    # READ INPUTS
    matrix_text <- trimws(input$calc_matrixA)
    vector_text <- trimws(input$calc_vectorb)
    guess_text  <- trimws(input$calc_initial_guess)
    tol <- input$calc_tolerance
    max_iter <- input$calc_max_iter
    
    # BASIC EMPTY CHECKS
    if (matrix_text == "") errors$A <- "Matrix A is required."
    if (vector_text == "") errors$b <- "Vector b is required."
    
    # PARSERS
    parse_matrix <- function(text) {
      
      rows <- strsplit(text, ";")[[1]]
      
      rows <- lapply(rows, function(r) {
        nums <- unlist(strsplit(trimws(r), "[ ,]+"))
        as.numeric(nums)
      })
      
      if (length(unique(sapply(rows, length))) != 1) {
        stop("Each row must have the same number of elements.")
      }
      
      matrix(unlist(rows), nrow = length(rows), byrow = TRUE)
    }
    
    parse_vector <- function(text) {
      nums <- unlist(strsplit(text, "[ ,]+"))
      as.numeric(nums)
    }
    
    # SAFE PARSING
    A <- tryCatch(parse_matrix(matrix_text), error = function(e) {
      errors$A <- "Invalid matrix format."
      NULL
    })
    
    b <- tryCatch(parse_vector(vector_text), error = function(e) {
      errors$b <- "Invalid vector format."
      NULL
    })
    
    # VALIDATION: MATRIX A
    if (!is.null(A)) {
      
      if (nrow(A) != ncol(A)) {
        errors$A <- "Matrix A must be square."
      }
      
      if (any(is.na(A))) {
        errors$A <- "Matrix A contains invalid numbers."
      }
    }
    
    # VALIDATION: VECTOR B
    if (!is.null(A) && !is.null(b)) {
      
      if (length(b) != nrow(A)) {
        errors$b <- "Vector b must match number of rows in A."
      }
      
      if (any(is.na(b))) {
        errors$b <- "Vector b contains invalid numbers."
      }
    }
    
    # INITIAL GUESS
    if (guess_text == "") {
      
      x0 <- if (!is.null(A)) rep(0, nrow(A)) else NULL
      
    } else {
      
      x0 <- tryCatch(parse_vector(guess_text), error = function(e) {
        errors$guess <- "Invalid initial guess format."
        NULL
      })
      
      if (!is.null(A) && !is.null(x0)) {
        if (length(x0) != nrow(A)) {
          errors$guess <- "Initial guess must match size of A."
        }
      }
    }
    
    # NUMERIC CHECKS
    if (!is.null(tol) && tol <= 0) {
      errors$tolerance <- "Tolerance must be > 0."
    }
    
    if (!is.null(max_iter) && max_iter <= 0) {
      errors$iter <- "Max iterations must be > 0."
    }
    
    # STOP IF ERRORS EXIST
    if (length(errors) > 0) {
      
      output$calculatorSolutionUI <- renderUI({
        div(class = "calc-error-block",
            lapply(errors, function(msg) {
              div(class = "calc-error-message", msg)
            })
        )
      })
      
      return()
    }
    
    # DIAGONAL DOMINANCE CHECK
    is_dd <- function(A) {
      for (i in 1:nrow(A)) {
        if (abs(A[i,i]) < sum(abs(A[i,])) - abs(A[i,i])) {
          return(FALSE)
        }
      }
      TRUE
    }
    
    if (!is_dd(A)) {
      
      output$calculatorSolutionUI <- renderUI({
        div(class = "calc-error-block",
            div(class = "calc-error-message",
                "Matrix A is not diagonally dominant. Cannot proceed."
            )
        )
      })
      
      return()
    }
    
    # MATRIX DECOMPOSITION
    D <- diag(diag(A))
    
    L <- A
    L[upper.tri(L, diag = TRUE)] <- 0
    
    U <- A
    U[lower.tri(U, diag = TRUE)] <- 0
    
    # STORE RESULTS (FOR UI)
    state$A <- A
    state$b <- b
    state$x0 <- x0
    state$D <- D
    state$L <- L
    state$U <- U
    
    # SUCCESS UI TEST
    output$calculatorSolutionUI <- renderUI({
      
      tagList(
        
        div(class = "calc-success-message",
            "Input validated. Ready for iteration."
        ),
        
        div(class = "matrix-block",
            h4("D Matrix"),
            renderTable(D)
        ),
        
        div(class = "matrix-block",
            h4("L Matrix"),
            renderTable(L)
        ),
        
        div(class = "matrix-block",
            h4("U Matrix"),
            renderTable(U)
        )
        
      )
    })
    
  })

  selected_example <- reactive({
    example_bank[[state$method]][[state$page]]
  })

  output$exampleCounter <- renderText({
    paste0(state$page, " / 4")
  })

  output$leftArrowUI <- renderUI({
    tags$button(
      type = "button",
      class = if (state$page > 1) "nav-action-btn" else "nav-action-btn disabled",
      onclick = if (state$page > 1) "Shiny.setInputValue('page_prev', Date.now(), {priority: 'event'})" else NULL,
      disabled = if (state$page <= 1) NA else NULL,
      img(src = "assets/left-arrow-blue.svg", class = "arrow", alt = "Previous page")
    )
  })

  output$rightArrowUI <- renderUI({
    tags$button(
      type = "button",
      class = if (state$page < 4) "nav-action-btn" else "nav-action-btn disabled",
      onclick = if (state$page < 4) "Shiny.setInputValue('page_next', Date.now(), {priority: 'event'})" else NULL,
      disabled = if (state$page >= 4) NA else NULL,
      img(src = "assets/right-arrow-blue.svg", class = "arrow", alt = "Next page")
    )
  })

  output$exampleSubtitle <- renderText({
    selected_example()$subtitle
  })

  output$algoContent <- renderUI({
    tagList(
      div(class = "content-grid",
        div(class = "content-codebox",
          HTML(selected_example()$algo)
        )
      )
    )
  })

  output$systemContent <- renderUI({
    ex <- selected_example()
    tagList(
      div(class = "content-grid",
        div(class = "content-pair",
          div(class = "content-value",
            div(class = "content-label", "Equation 1"),
            tags$p(ex$system[1], style = "margin: 0;")
          ),
          div(class = "content-value",
            div(class = "content-label", "Equation 2"),
            tags$p(ex$system[2], style = "margin: 0;")
          )
        ),
        div(class = "content-note",
          div(class = "content-label", "System summary"),
          tags$p(HTML(paste0("<strong>Matrix A:</strong> ", ex$system[1], " / ", ex$system[2])), style = "margin: 0 0 0.5rem 0;"),
          tags$p(HTML("<strong>Vector b:</strong> selected from the example page"), style = "margin: 0;")
        )
      )
    )
  })

  output$guessContent <- renderUI({
    ex <- selected_example()
    tagList(
      div(class = "content-grid",
        div(class = "content-pair",
          numericInput("x1", "x₁ initial:", value = ex$guess[1], min = -10, max = 10),
          numericInput("x2", "x₂ initial:", value = ex$guess[2], min = -10, max = 10)
        ),
        div(class = "content-note", "Choose your starting point for the iteration process.")
      )
    )
  })

  output$iterContent <- renderUI({
    ex <- selected_example()
    tagList(
      div(class = "content-grid",
        numericInput("n_iter", "Number of iterations:", value = ex$iter, min = 1, max = 100),
        div(class = "content-note", "Define how many times the algorithm will be applied.")
      )
    )
  })

  output$convContent <- renderUI({
    ex <- selected_example()
    tagList(
      div(class = "content-grid",
        div(class = "content-label", "Convergence Plot"),
        div(class = "content-plotbox", plotOutput("convergencePlot", height = "250px")),
        div(class = "content-note", ex$conv)
      )
    )
  })

  output$errorContent <- renderUI({
    ex <- selected_example()
    tagList(
      div(class = "content-grid",
        div(class = "content-label", "Approximation Error"),
        div(class = "content-plotbox", plotOutput("errorPlot", height = "250px")),
        div(class = "content-tablebox", tableOutput("iterHistory")),
        div(class = "content-note", ex$err)
      )
    )
  })

  output$calculatorMethodButtons <- renderUI({
    tagList(
      div(class = "calculator-methods",
        tags$button(
          type = "button",
          class = if (state$calcMethod == "gauss") "method-btn calc-method-btn active" else "method-btn calc-method-btn",
          onclick = "Shiny.setInputValue('calc_method', 'gauss', {priority: 'event'})",
          "Gauss-Seidel"
        ),
        tags$button(
          type = "button",
          class = if (state$calcMethod == "jacobi") "method-btn calc-method-btn active" else "method-btn calc-method-btn",
          onclick = "Shiny.setInputValue('calc_method', 'jacobi', {priority: 'event'})",
          "Jacobi"
        )
      )
    )
  })

  output$calculatorTabs <- renderUI({
    tab_button <- function(tab_id, label) {
      tags$button(
        type = "button",
        class = if (state$calculatorTab == tab_id) "calc-tab-btn active" else "calc-tab-btn",
        onclick = sprintf("Shiny.setInputValue('calculatorTab', '%s', {priority: 'event'})", tab_id),
        label
      )
    }

    div(class = "solutions-tabs",
      tab_button("lud", "LU Matrix & Diagonal Dominance"),
      tab_button("iterations", "Iterations"),
      tab_button("solutions", "Solutions")
    )
  })

  output$calculatorSolutionUI <- renderUI({
    if (state$calculatorTab == "iterations") {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "Iterations"),
          div(class = "iteration-list",
            div(class = "iteration-row", span("Iteration 1"), span("Pending")),
            div(class = "iteration-row", span("Iteration 2"), span("Pending")),
            div(class = "iteration-row", span("Iteration 3"), span("Pending"))
          ),
          div(class = "solution-note", "Enter the matrix values and choose a method to see the iteration results here.")
        )
      )
    } else if (state$calculatorTab == "solutions") {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "Solutions"),
          div(class = "solutions-card",
          )
        )
      )
    } else {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "LU Factorization"),
          div(class = "lu-diagram",
            div(class = "factor-card",
              div(class = "factor-label", "L"),
              div(class = "matrix-view",
                div(class = "matrix-placeholder-grid",
                  span(), span(), span(),
                  span(), span(), span(),
                  span(), span(), span()
                )
              )
            ),
            div(class = "factor-card",
              div(class = "factor-label", "U"),
              div(class = "matrix-view",
                div(class = "matrix-placeholder-grid",
                  span(), span(), span(),
                  span(), span(), span(),
                  span(), span(), span()
                )
              )
            )
          ),
          div(class = "solution-note", "The LU result preview will appear here after the calculator inputs are provided.")
        )
      )
    }
  })

  result <- reactive({
    ex <- selected_example()
    x1 <- input$x1
    x2 <- input$x2
    if (is.null(x1)) x1 <- ex$guess[1]
    if (is.null(x2)) x2 <- ex$guess[2]
    niter <- input$n_iter
    if (is.null(niter)) niter <- ex$iter
    x0 <- c(x1, x2)
    if (isolate(state$method) == "gauss") {
      gauss_seidel_iterations(ex$A, ex$b, x0, niter)
    } else {
      jacobi_iterations(ex$A, ex$b, x0, niter)
    }
  })

  output$convergencePlot <- renderPlot({
    res <- result()
    h <- as.matrix(res$history)
    iters <- 0:(nrow(h) - 1)
    method_name <- if (isolate(state$method) == "gauss") "Gauss-Seidel" else "Jacobi"

    matplot(iters, h, type = "b", pch = 19, lty = 1,
            col = c("steelblue", "firebrick"),
            xlab = "Iteration", ylab = "Value",
            main = paste0(method_name, " Iteration Convergence"))
    legend("right", legend = colnames(h),
           col = c("steelblue", "firebrick"),
           lty = 1, pch = 19, bty = "n")
    grid()
  })

  output$errorPlot <- renderPlot({
    res <- result()
    errors <- res$errors[-1]
    iters <- seq_along(errors)

    plot(iters, errors, type = "b", pch = 19, col = "darkred",
         xlab = "Iteration", ylab = "Error",
         main = "Approximation Error")
    grid()
  })

  output$iterHistory <- renderTable({
    res <- result()
    h <- res$history
    errors <- res$errors

    data.frame(
      Iteration = 0:(nrow(h) - 1),
      x1 = round(h$x1, 6),
      x2 = round(h$x2, 6),
      Error = round(errors, 8)
    )
  })

}

shinyApp(ui = ui, server = server)