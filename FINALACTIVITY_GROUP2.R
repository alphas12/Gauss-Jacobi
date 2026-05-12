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
  list("When solving ", span("diagonally dominant systems", class = "highlighted-text"), " where fast convergence is needed."),
  
  list("For engineering and scientific problems that involve ", 
       span("large sparse matrices", class = "highlighted-text"), 
       " and repeated calculations."),
  
  list("When updated variable values should immediately improve the next approximation during iteration."),
  
  list("In numerical simulations where ", 
       span("memory efficiency", class = "highlighted-text"), 
       " is important."),
  
  list("When comparing iterative methods because Gauss-Seidel often converges ", 
       span("faster than Jacobi", class = "highlighted-text"), ".")
)

uses_jacobi <- list(
  list("When solving systems that can be separated into ", 
       span("independent calculations", class = "highlighted-text"), "."),
  
  list("For problems suited to ", 
       span("parallel computing", class = "highlighted-text"), 
       " since all updates use previous iteration values."),
  
  list("When introducing iterative numerical methods because the algorithm is simple and easy to understand."),
  
  list("For diagonally dominant matrices where stable convergence can still be achieved."),
  
  list("When comparing convergence behavior with ", 
       span("Gauss-Seidel", class = "highlighted-text"), 
       " in numerical analysis.")
)

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "styles.css"
    )
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
          "The ", span("Gauss-Seidel", class = "highlighted-text"),
          " and ", span("Jacobi", class = "highlighted-text"),
          " methods are iterative numerical techniques used to solve systems of linear equations. 
          These methods generate approximate solutions through repeated computation and are especially useful 
          for large systems where direct methods become computationally expensive. Both approaches are widely 
          applied in engineering, physics, computer science, and numerical analysis."
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
            "The ", span("Gauss-Seidel Method", class = "highlighted-text"),
            " is an iterative technique for solving systems of linear equations. 
            Unlike Jacobi, this method immediately uses newly computed values within the same iteration, 
            allowing faster convergence for many diagonally dominant systems. It is commonly used in numerical 
            computing because of its efficiency and simplicity."
          )
        ),
        div(style= "flex:1; background:#183D5E;", class="definitions-main-content",
          h1(style = "color:#F6F7F8;", class="definitions-title", "History"),
          p(style = "color: #F6F7F8;", class = "body-text",
            "The method was developed from the works of ", 
            span("Carl Friedrich Gauss", class = "highlighted-text"),
            " and later refined by ", 
            span("Philipp Ludwig von Seidel", class = "highlighted-text"),
            ". It became one of the most important iterative techniques in numerical linear algebra and remains 
            widely used in modern computational mathematics."
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
                "The ", span("Jacobi Method", class = "highlighted-text"),
                " was introduced by German mathematician Carl Gustav Jacob Jacobi. 
                It is one of the earliest iterative techniques for solving linear systems and became important in 
                numerical analysis due to its simplicity and suitability for parallel computation."
              )
          ),
          div(style= "width:73%; background:#183D5E;", class="definitions-main-content",
              h1(style = "color: #F6F7F8", class="definitions-title", "Jacobi Method"),
              p(style = "color: #F6F7F8", class = "body-text",
                "The ", span("Jacobi Method", class = "highlighted-text"),
                " is an iterative algorithm used to solve systems of linear equations. 
                Each variable is updated using only values from the previous iteration, making the method simple 
                and highly suitable for parallel processing. Although it may converge slower than Gauss-Seidel, 
                it remains an important foundational numerical technique."
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
                "Both the ", span("Gauss-Seidel", class = "highlighted-text"),
                " and ", span("Jacobi", class = "highlighted-text"),
                " methods are based on transforming a system of linear equations into iterative formulas. 
                A system written in matrix form as Ax = b is rearranged so that each variable can be repeatedly 
                approximated until convergence is achieved. These methods rely heavily on concepts such as matrix 
                decomposition, diagonal dominance, convergence criteria, and iterative approximation."
              )
          )
      ),
          
  ),
  
  # Conditions Page
  div(id = "conditions",
      div(class="conditions-content", 
          div(class="conditions-subcontent", 
              div(class="conditions-title ", "Conditions"),
              p(class = "body-text",
                "Both the ", span("Gauss-Seidel", class = "highlighted-text"),
                " and ", span("Jacobi", class = "highlighted-text"),
                " methods are based on transforming a system of linear equations into iterative formulas. 
                A system written in matrix form as Ax = b is rearranged so that each variable can be repeatedly 
                approximated until convergence is achieved. These methods rely heavily on concepts such as matrix 
                decomposition, diagonal dominance, convergence criteria, and iterative approximation."
              )
          ),
          div(class="conditions-subcontent", 
              div(class="conditions-subtitle ", "Convergence"),
              p(class = "body-text",
                "A method is said to converge when successive approximations become increasingly close to the exact solution. 
                For both Jacobi and Gauss-Seidel methods, convergence is commonly guaranteed when the coefficient matrix is ",
                span("strictly diagonally dominant", class = "highlighted-text"),
                " or symmetric positive definite."
              )
          ),
          div(class="conditions-subcontent", 
              div(class="conditions-subtitle ", "Divergence"),
              p(class = "body-text",
                "Divergence occurs when the iterative approximations move away from the actual solution instead of approaching it. 
                This commonly happens when the matrix does not satisfy convergence conditions or when numerical instability affects 
                the iteration process."
              )
          ),
      ),
      div(style = "background: #183D5E; border-radius: 20px; margin: 60px 50px; margin-left: 0;", class="conditions-content",
          div(style="color: #F6F7F8;", class="conditions-subtitle ", "Convergence"),
          p(style="color: #F6F7F8;", class = "body-text",
            "In practice, the ", span("Gauss-Seidel Method", class = "highlighted-text"),
            " often converges faster because it immediately uses updated values during computation. 
            The ", span("Jacobi Method", class = "highlighted-text"),
                      " is easier to parallelize because all updates depend only on values from the previous iteration. 
            Choosing the appropriate method depends on the structure of the matrix and the computational requirements."
          )
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

run_iterations <- function(A, b, x0, tol, max_iter, method = "gauss") {

  n <- length(b)

  history <- matrix(0, nrow = max_iter + 1, ncol = n)
  history[1, ] <- x0

  errors <- numeric(max_iter + 1)
  errors[1] <- NA

  x <- x0

  converged <- FALSE

  for (k in 1:max_iter) {

    x_old <- x

    if (method == "gauss") {

      # GAUSS-SEIDEL
      for (i in 1:n) {

        sum1 <- if (i > 1) sum(A[i, 1:(i-1)] * x[1:(i-1)]) else 0
        sum2 <- if (i < n) sum(A[i, (i+1):n] * x_old[(i+1):n]) else 0

        x[i] <- (b[i] - sum1 - sum2) / A[i, i]
      }

    } else {

      # JACOBI
      x_new <- numeric(n)

      for (i in 1:n) {

        sum_others <- sum(A[i, -i] * x_old[-i])

        x_new[i] <- (b[i] - sum_others) / A[i, i]
      }

      x <- x_new
    }

    err <- sqrt(sum((x - x_old)^2))

    history[k + 1, ] <- x
    errors[k + 1] <- err

    if (err < tol) {
      converged <- TRUE
      break
    }
  }

  used_rows <- 1:(k + 1)

  history <- history[used_rows, , drop = FALSE]
  errors <- errors[used_rows]

  colnames(history) <- paste0("x", 1:n)

  list(
    solution = x,
    history = as.data.frame(history),
    errors = errors,
    iterations = k,
    converged = converged
  )
}

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

  state <- reactiveValues(method = "gauss", page = 1, calcMethod = "gauss", calculatorTab = "lud", calcResult = NULL)

  
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

    calc_res <- run_iterations(
      A = A,
      b = b,
      x0 = x0,
      tol = tol,
      max_iter = max_iter,
      method = state$calcMethod
    )

    state$calcResult <- calc_res
    
    # SUCCESS UI TEST
    output$calculatorSolutionUI <- renderUI({
      
      tagList(
        
        div(class = "calc-success-message",
            "Input validated. Ready for iteration."
        ),
        
        div(class = "matrix-block",
            h4("D Matrix"),
            tableOutput("DTable")
        ),
        
        div(class = "matrix-block",
            h4("L Matrix"),
            tableOutput("LTable")
        ),
        
        div(class = "matrix-block",
            h4("U Matrix"),
            tableOutput("UTable")
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
      res <- state$calcResult

      if (is.null(res)) {
        return(div("No iteration data yet."))
      }

      tagList(

        h3(class = "solution-heading", "Iterations"),
        tableOutput("calcIterationsTable"),
        plotOutput("calcErrorPlot", height = "300px")

      )
  }else if (state$calculatorTab == "solutions") {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "Solutions"),
          {
            req(state$calcResult)
            
            res <- state$calcResult
            
            div(class = "solutions-card",
                
                h4("Final Approximation"),
                
                renderTable({
                  
                  data.frame(
                    Variable = paste0("x", 1:length(res$solution)),
                    Value = round(res$solution, 8)
                  )
                  
                }),
                
                br(),
                
                p(
                  strong("Iterations Used: "),
                  res$iterations
                ),
                
                p(
                  strong("Converged: "),
                  ifelse(res$converged, "YES", "NO")
                )
            )
          }
        )
      )
    } else {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "LU Factorization"),
          div(class = "lu-diagram",
              div(class = "lu-diagram",
                  div(class = "factor-card",
                      div(class = "factor-label", "L"),
                      div(class = "matrix-view",
                          tableOutput("LTable")
                      )
                  ),
                  
                  div(class = "factor-card",
                      div(class = "factor-label", "U"),
                      div(class = "matrix-view",
                          tableOutput("UTable")
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

  output$calcIterationsTable <- renderTable({
    
    req(state$calcResult)
    
    h <- state$calcResult$history
    e <- state$calcResult$errors
    
    data.frame(
      Iteration = 0:(nrow(h)-1),
      round(h, 6),
      Error = round(e, 8)
    )
  })
  
  output$calcErrorPlot <- renderPlot({
    
    req(state$calcResult)
    
    errors <- state$calcResult$errors[-1]
    
    plot(
      seq_along(errors),
      errors,
      type = "b",
      pch = 19,
      col = "darkred",
      xlab = "Iteration",
      ylab = "Error",
      main = "Convergence Error"
    )
    
    grid()
  })
  
  output$DTable <- renderTable({
    state$D
  })
  
  output$LTable <- renderTable({
    state$L
  })
  
  output$UTable <- renderTable({
    state$U
  })
}

shinyApp(ui = ui, server = server)