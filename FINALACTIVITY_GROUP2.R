# ==============================================================================
# Jacobi Iteration Learning Application
# ==============================================================================
# Description:
#   This Shiny application introduces iterative methods and provides a Jacobi
#   Iteration calculator. The calculator accepts a system of linear equations,
#   an initial guess, tolerance, and maximum iterations, then displays:
#     1. Matrix decomposition and diagonal dominance result
#     2. Step-by-step Jacobi iterations in LaTeX
#     3. Final solution vector, final error, and convergence status
#
# Notes:
#   - The calculator itself is Jacobi-only.
# ===============================================================================

library(shiny)
library(gtools)


# ==============================================================================
# 1. Numerical Utility Functions
# ===============================================================================

#' Check if a matrix is diagonally dominant.
#'
#' A matrix is diagonally dominant when each diagonal value is at least as large
#' as the sum of the absolute values of the other entries in the same row.
#'
#' @param mat Numeric matrix.
#' @return TRUE if the matrix is diagonally dominant; otherwise FALSE.
is_diagonally_dominant <- function(mat) {
  if (nrow(mat) != ncol(mat)) {
    message("Matrix is not square. Diagonal dominance is not applicable.")
    return(FALSE)
  }
  
  for (i in seq_len(nrow(mat))) {
    diagonal_value <- abs(mat[i, i])
    off_diagonal_sum <- sum(abs(mat[i, -i]))
    
    if (is.na(diagonal_value) || diagonal_value < off_diagonal_sum) {
      return(FALSE)
    }
  }
  
  TRUE
}

#' Find a row order that makes a matrix diagonally dominant.
#'
#' This function first checks the original matrix. If it is not diagonally
#' dominant, it checks all possible row permutations. This is acceptable here
#' because the calculator limits the number of variables to a small value.
#'
#' @param mat Numeric matrix.
#' @return A vector of row indices if a valid order exists; otherwise NULL.
find_dominant_order <- function(mat) {
  n <- nrow(mat)
  
  if (is_diagonally_dominant(mat)) {
    return(seq_len(n))
  }
  
  all_orders <- permutations(n, n, seq_len(n))
  
  for (i in seq_len(nrow(all_orders))) {
    current_order <- all_orders[i, ]
    test_matrix <- mat[current_order, , drop = FALSE]
    
    if (is_diagonally_dominant(test_matrix)) {
      return(current_order)
    }
  }
  
  NULL
}

#' Split matrix A into L, D, and U matrices.
#'
#' For the Jacobi method, A is decomposed as:
#'   A = L + D + U
#' where L is the strictly lower triangular part, D is the diagonal matrix,
#' and U is the strictly upper triangular part.
#'
#' @param mat Square numeric matrix.
#' @return A list containing L, D, and U.
extract_LDU <- function(mat) {
  n <- nrow(mat)
  
  L <- matrix(0, nrow = n, ncol = n)
  D <- matrix(0, nrow = n, ncol = n)
  U <- matrix(0, nrow = n, ncol = n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i > j) {
        L[i, j] <- mat[i, j]
      } else if (i < j) {
        U[i, j] <- mat[i, j]
      } else {
        D[i, j] <- mat[i, j]
      }
    }
  }
  
  list(L = L, D = D, U = U)
}

#' Run the Jacobi method and store detailed iteration data.
#'
#' The Jacobi update used is:
#'   x^(k+1) = D^(-1) [ b - (L + U)x^(k) ]
#'
#' @param A Coefficient matrix.
#' @param b Right-hand side vector.
#' @param x0 Initial guess vector.
#' @param tol Tolerance for the infinity-norm error.
#' @param max_iter Maximum number of iterations.
#' @return A list containing the solution, convergence status, and iteration log.
jacobi_detailed <- function(A, b, x0, tol = 0.005, max_iter = 1000) {
  decomposition <- extract_LDU(A)
  L <- decomposition$L
  D <- decomposition$D
  U <- decomposition$U
  
  if (any(diag(D) == 0)) {
    stop("Zero found in diagonal. Jacobi cannot proceed.")
  }
  
  x_old <- x0
  iterations_detail <- list()
  
  # Store iteration 0 so the UI can show the initial state if needed.
  iterations_detail[[1]] <- list(
    k = 0,
    x = x_old,
    error = NA,
    D_inv_computed = solve(D),
    L_plus_U = L + U,
    b_minus_LU_x = NA
  )
  
  converged <- FALSE
  final_k <- 0
  
  for (k in seq_len(max_iter)) {
    D_inv <- solve(D)
    L_plus_U <- L + U
    b_minus_LU_x <- b - L_plus_U %*% x_old
    x_new <- D_inv %*% b_minus_LU_x
    error <- max(abs(x_new - x_old))
    
    iterations_detail[[k + 1]] <- list(
      k = k,
      x = x_new,
      x_old = x_old,
      error = error,
      D_inv_computed = D_inv,
      L_plus_U = L_plus_U,
      b_minus_LU_x = b_minus_LU_x,
      b = b
    )
    
    x_old <- x_new
    final_k <- k
    
    if (error < tol) {
      converged <- TRUE
      break
    }
  }
  
  list(
    solution = x_old,
    iterations = final_k,
    converged = converged,
    final_error = if (final_k > 0) iterations_detail[[final_k + 1]]$error else NA,
    D = D,
    L = L,
    U = U,
    iterations_detail = iterations_detail
  )
}

#' Run Jacobi iterations for the educational example section.
#'
#' This version stores a compact history table used by the example plots.
#'
#' @param A Coefficient matrix.
#' @param b Right-hand side vector.
#' @param x0 Initial guess vector.
#' @param tol Tolerance for stopping.
#' @param max_iter Maximum number of iterations.
#' @return A list containing history, errors, and convergence status.
run_jacobi_example_iterations <- function(A, b, x0, tol = 1e-10, max_iter = 10) {
  n <- length(b)
  history <- matrix(0, nrow = max_iter + 1, ncol = n)
  errors <- numeric(max_iter + 1)
  
  history[1, ] <- x0
  errors[1] <- NA
  
  x <- x0
  converged <- FALSE
  
  for (k in seq_len(max_iter)) {
    x_old <- x
    x_new <- numeric(n)
    
    for (i in seq_len(n)) {
      sum_others <- sum(A[i, -i] * x_old[-i])
      x_new[i] <- (b[i] - sum_others) / A[i, i]
    }
    
    x <- x_new
    err <- sqrt(sum((x - x_old)^2))
    
    history[k + 1, ] <- x
    errors[k + 1] <- err
    
    if (err < tol) {
      converged <- TRUE
      break
    }
  }
  
  used_rows <- seq_len(k + 1)
  history <- history[used_rows, , drop = FALSE]
  errors <- errors[used_rows]
  colnames(history) <- paste0("x", seq_len(n))
  
  list(
    solution = x,
    history = as.data.frame(history),
    errors = errors,
    iterations = k,
    converged = converged
  )
}

# ==============================================================================
# 2. Input Parsing and Formatting Helpers
# ===============================================================================

#' Return variable names used by the equation parser.
#'
#' For n <= 6, common algebraic variables are used: x, y, z, w, v, u.
#' For larger systems, x1, x2, ... would be used, although the UI limits n.
get_variable_names <- function(n) {
  common_vars <- c("x", "y", "z", "w", "v", "u")
  
  if (n <= length(common_vars)) {
    return(common_vars[seq_len(n)])
  }
  
  paste0("x", seq_len(n))
}

#' Parse one side of a linear equation.
#'
#' Examples accepted by the parser:
#'   - "10x + 2y - 3z"
#'   - "-x + 5y - 3z"
#'   - "2x + 0y + 3z"
#'
#' @param expr Character expression without the equals sign.
#' @param vars Variable names expected in the system.
#' @return A list with coefficient vector and constant value.
parse_linear_expression <- function(expr, vars) {
  expr <- gsub("\\s+", "", expr)
  expr <- gsub("−", "-", expr)
  expr <- gsub("\\*", "", expr)
  
  if (expr == "") {
    stop("Empty expression found.")
  }
  
  terms <- regmatches(expr, gregexpr("[+-]?[^+-]+", expr, perl = TRUE))[[1]]
  coeffs <- setNames(rep(0, length(vars)), vars)
  constant <- 0
  
  # Longer variable names should be checked first to avoid partial matches.
  vars_ordered <- vars[order(nchar(vars), decreasing = TRUE)]
  
  for (term in terms) {
    matched_var <- NULL
    
    for (v in vars_ordered) {
      if (grepl(paste0(v, "$"), term)) {
        matched_var <- v
        break
      }
    }
    
    if (!is.null(matched_var)) {
      coef_text <- sub(paste0(matched_var, "$"), "", term)
      
      coef <- if (coef_text == "" || coef_text == "+") {
        1
      } else if (coef_text == "-") {
        -1
      } else {
        suppressWarnings(as.numeric(coef_text))
      }
      
      if (is.na(coef)) {
        stop(paste("Invalid coefficient in term:", term))
      }
      
      coeffs[matched_var] <- coeffs[matched_var] + coef
    } else {
      value <- suppressWarnings(as.numeric(term))
      
      if (is.na(value)) {
        stop(paste("Invalid term found:", term))
      }
      
      constant <- constant + value
    }
  }
  
  list(coeffs = as.numeric(coeffs), constant = constant)
}

#' Convert a typed system of equations into A and b.
#'
#' The parser moves all variable terms to the left side and constants to the
#' right side, producing Ax = b.
#'
#' @param text Multi-line equation input from the calculator.
#' @param n Expected number of equations and variables.
#' @return A list containing A, b, and variable names.
parse_equation_system <- function(text, n) {
  vars <- get_variable_names(n)
  text <- gsub("−", "-", text)
  
  rows <- unlist(strsplit(text, "[\n;]+"))
  rows <- trimws(rows)
  rows <- rows[nzchar(rows)]
  
  if (length(rows) != n) {
    stop(paste0("Expected ", n, " equations, but found ", length(rows), "."))
  }
  
  A <- matrix(0, nrow = n, ncol = n)
  b <- numeric(n)
  
  for (i in seq_len(n)) {
    parts <- unlist(strsplit(rows[i], "=", fixed = TRUE))
    
    if (length(parts) != 2) {
      stop(paste("Equation", i, "must contain exactly one '=' sign."))
    }
    
    left <- parse_linear_expression(parts[1], vars)
    right <- parse_linear_expression(parts[2], vars)
    
    # Convert equation to standard form:
    #   left_variables - right_variables = right_constants - left_constants
    A[i, ] <- left$coeffs - right$coeffs
    b[i] <- right$constant - left$constant
  }
  
  colnames(A) <- vars
  list(A = A, b = b, vars = vars)
}

#' Parse the initial guess vector entered by the user.
#'
#' The user may separate values with commas, spaces, semicolons, or new lines.
parse_initial_guess <- function(text, n) {
  text <- trimws(text)
  
  if (text == "") {
    return(rep(0, n))
  }
  
  text <- gsub("[;\n]+", ",", text)
  values <- unlist(strsplit(text, "[, ]+"))
  values <- values[nzchar(values)]
  
  x0 <- suppressWarnings(as.numeric(values))
  
  if (any(is.na(x0))) {
    stop("Initial guess contains invalid numbers.")
  }
  
  if (length(x0) != n) {
    stop(paste0("Initial guess must contain exactly ", n, " values."))
  }
  
  x0
}

#' Format small numbers and decimals for display.
format_num <- function(x, digits = 4) {
  ifelse(
    abs(x) < 1e-12,
    "0",
    formatC(x, format = "fg", digits = digits)
  )
}

#' Convert a matrix or vector into a LaTeX bmatrix.
matrix_to_latex <- function(M) {
  if (is.vector(M)) {
    M <- matrix(M, ncol = 1)
  }
  
  rows <- apply(M, 1, function(row) {
    paste(format_num(row), collapse = " & ")
  })
  
  paste0(
    "\\begin{bmatrix}",
    paste(rows, collapse = " \\\\ "),
    "\\end{bmatrix}"
  )
}

#' Convert a vector into a vertical LaTeX bmatrix.
vector_to_latex <- function(v) {
  matrix_to_latex(matrix(as.numeric(v), ncol = 1))
}

#' Convert an integer into Unicode subscript digits.
#'
#' Used for displaying variables as x₁, x₂, ..., xₙ in tables.
to_subscript <- function(num) {
  sub_map <- c(
    "0" = "\u2080",
    "1" = "\u2081",
    "2" = "\u2082",
    "3" = "\u2083",
    "4" = "\u2084",
    "5" = "\u2085",
    "6" = "\u2086",
    "7" = "\u2087",
    "8" = "\u2088",
    "9" = "\u2089"
  )
  
  sapply(as.character(num), function(x) {
    digits <- strsplit(x, "")[[1]]
    paste0(sub_map[digits], collapse = "")
  })
}


# ==============================================================================
# 3. Static Content for Educational Sections
# ===============================================================================

uses_gs <- list(
  list("When solving ", span("diagonally dominant systems", class = "highlighted-text"), " where fast convergence is needed."),
  list("For engineering and scientific problems that involve ", span("large sparse matrices", class = "highlighted-text"), " and repeated calculations."),
  list("When updated variable values should immediately improve the next approximation during iteration."),
  list("In numerical simulations where ", span("memory efficiency", class = "highlighted-text"), " is important."),
  list("When comparing iterative methods because Gauss-Seidel often converges ", span("faster than Jacobi", class = "highlighted-text"), ".")
)

uses_jacobi <- list(
  list("When solving systems that can be separated into ", span("independent calculations", class = "highlighted-text"), "."),
  list("For problems suited to ", span("parallel computing", class = "highlighted-text"), " since all updates use previous iteration values."),
  list("When introducing iterative numerical methods because the algorithm is simple and easy to understand."),
  list("For diagonally dominant matrices where stable convergence can still be achieved."),
  list("When studying convergence behavior in iterative numerical methods.")
)

#' Return the example cases used in the Examples section.
#'
#' Each example is intentionally 2x2 because the example plots and controls are
#' designed for two variables. The calculator below supports 2 to 6 variables.
get_example_bank <- function() {
  list(
    list(
      algo = paste0(
        "<h5>Jacobi Iteration Formula:</h5>",
        
        "<div class='formula-display'>",
        "$$\\mathbf{x}^{(k+1)} = \\mathbf{D}^{-1}\\left(\\mathbf{b} - (\\mathbf{L} + \\mathbf{U})\\mathbf{x}^{(k)}\\right)$$",
        "</div>",
        
        "<p>In component form:</p>",
        
        "<div class='formula-display'>",
        "$$x_i^{(k+1)} = \\frac{b_i - \\sum_{j \\ne i} a_{ij}x_j^{(k)}}{a_{ii}}$$",
        "</div>",
        
        "<p>All variables are updated using values from the previous iteration only.</p>"
      ),
      system = c("4x₁ + x₂ = 1", "2x₁ + 3x₂ = 2"),
      A = matrix(c(4, 1, 2, 3), nrow = 2, byrow = TRUE),
      b = c(1, 2),
      guess = c(0, 0),
      iter = 4,
      conv = "The method converges gradually because each update uses the full previous vector.",
      err = "The error is measured using the distance between successive iteration vectors."
    ),
    list(
      algo = "<h5>Key Idea:</h5><p>Jacobi uses the full previous vector for every update, making the method easy to understand and suitable for parallel computation.</p>",
      system = c("3x₁ - x₂ = 4", "x₁ + 5x₂ = 12"),
      A = matrix(c(3, -1, 1, 5), nrow = 2, byrow = TRUE),
      b = c(4, 12),
      guess = c(1, 1),
      iter = 5,
      conv = "The values move toward the solution through repeated simultaneous updates.",
      err = "Residuals shrink gradually as the updates repeat."
    ),
    list(
      algo = "<h5>Reminder:</h5><p>Jacobi is often used as an introductory method for studying iterative solvers.</p>",
      system = c("6x₁ + x₂ = 7", "x₁ + 4x₂ = 8"),
      A = matrix(c(6, 1, 1, 4), nrow = 2, byrow = TRUE),
      b = c(7, 8),
      guess = c(0, 0),
      iter = 4,
      conv = "The sequence updates both variables simultaneously at each iteration.",
      err = "The approximation error is visible in the table of iterates."
    ),
    list(
      algo = "<h5>Practice Case:</h5><p>Use this example to observe how the Jacobi sequence approaches the solution step by step.</p>",
      system = c("5x₁ - 2x₂ = 4", "2x₁ + 6x₂ = 18"),
      A = matrix(c(5, -2, 2, 6), nrow = 2, byrow = TRUE),
      b = c(4, 18),
      guess = c(0, 0),
      iter = 6,
      conv = "The iterates slowly approach the solution because all values are updated from the previous iteration.",
      err = "The error table makes it easy to track the change from one iteration to the next."
    )
  )
}


# ==============================================================================
# 4. UI Helper Functions
# ===============================================================================

#' Create the fixed navigation bar.
create_navbar <- function() {
  div(
    class = "custom-navbar",
    a("Landing", href = "#landing", class = "nav-item"),
    a("Definitions", href = "#definitions-gs", class = "nav-item"),
    a("Foundations", href = "#foundations", class = "nav-item"),
    a("Conditions", href = "#conditions", class = "nav-item"),
    a("Examples", href = "#examples", class = "nav-item"),
    a("Calculator", href = "#calculator", class = "nav-item")
  )
}

#' Create the landing section.
create_landing_section <- function() {
  div(
    id = "landing",
    img(src = "assets/landing-photo.png"),
    div(
      class = "landing-right",
      img(src = "assets/title.svg", style = "display: block;"),
      p(
        style = "text-align: justify; margin: 0;",
        class = "body-text",
        "The ", span("Gauss-Seidel", class = "highlighted-text"),
        " and ", span("Jacobi", class = "highlighted-text"),
        " methods are iterative numerical techniques used to solve systems of linear equations. These methods generate approximate solutions through repeated computation and are especially useful for large systems where direct methods become computationally expensive. Both approaches are widely applied in engineering, physics, computer science, and numerical analysis."
      )
    )
  )
}

#' Create the definition sections for Gauss-Seidel and Jacobi.
create_definitions_section <- function() {
  tagList(
    div(
      id = "definitions-gs",
      class = "definitions",
      div(
        class = "definitions-gs-left",
        div(
          style = "width:73%;",
          class = "definitions-main-content",
          h1(class = "definitions-title", "Gauss-Seidel Method"),
          p(
            class = "body-text",
            "The ", span("Gauss-Seidel Method", class = "highlighted-text"),
            " is an iterative technique for solving systems of linear equations. Unlike Jacobi, this method immediately uses newly computed values within the same iteration, allowing faster convergence for many diagonally dominant systems. It is commonly used in numerical computing because of its efficiency and simplicity."
          )
        ),
        div(
          style = "flex:1; background:#183D5E;",
          class = "definitions-main-content",
          h1(style = "color:#F6F7F8;", class = "definitions-title", "History"),
          p(
            style = "color: #F6F7F8;",
            class = "body-text",
            "The method was developed from the works of ",
            span("Carl Friedrich Gauss", class = "highlighted-text"),
            " and later refined by ",
            span("Philipp Ludwig von Seidel", class = "highlighted-text"),
            ". It became one of the most important iterative techniques in numerical linear algebra and remains widely used in modern computational mathematics."
          )
        )
      ),
      div(
        class = "definitions-sub-content",
        div(
          class = "definitions-sub-content-title",
          img(src = "assets/quote.svg", style = "display: block; width:40px; height:auto"),
          h1(class = "definitions-title", "When do we use it?")
        ),
        div(
          class = "definitions-sub-content-itemlist",
          lapply(uses_gs, function(item) {
            div(
              class = "definitions-gs-sub-content-item",
              p(style = "text-align: right;", class = "body-text", item)
            )
          })
        )
      )
    ),
    div(
      id = "definitions-jacobi",
      style = "padding: 0;",
      class = "definitions",
      div(
        class = "definitions-sub-content",
        div(
          style = "align-items: end;",
          class = "definitions-sub-content-title",
          img(src = "assets/quote.svg", style = "display: block; width:40px; height:auto"),
          h1(style = "text-align: end;", class = "definitions-title", "When do we use it?")
        ),
        div(
          class = "definitions-sub-content-itemlist",
          lapply(uses_jacobi, function(item) {
            div(class = "definitions-jacobi-sub-content-item", p(class = "body-text", item))
          })
        )
      ),
      div(
        class = "definitions-jacobi-right",
        div(
          style = "flex:1;",
          class = "definitions-main-content",
          h1(class = "definitions-title", "History"),
          p(
            class = "body-text",
            "The ", span("Jacobi Method", class = "highlighted-text"),
            " was introduced by German mathematician Carl Gustav Jacob Jacobi. It is one of the earliest iterative techniques for solving linear systems and became important in numerical analysis due to its simplicity and suitability for parallel computation."
          )
        ),
        div(
          style = "width:73%; background:#183D5E;",
          class = "definitions-main-content",
          h1(style = "color: #F6F7F8", class = "definitions-title", "Jacobi Method"),
          p(
            style = "color: #F6F7F8",
            class = "body-text",
            "The ", span("Jacobi Method", class = "highlighted-text"),
            " is an iterative algorithm used to solve systems of linear equations. Each variable is updated using only values from the previous iteration, making the method simple and highly suitable for parallel processing. Although it may converge slower than Gauss-Seidel, it remains an important foundational numerical technique."
          )
        )
      )
    )
  )
}

#' Create the mathematical foundations section.
create_foundations_section <- function() {
  div(
    id = "foundations",
    div(
      class = "foundations-content-main",
      div(class = "foundations-content", h1(class = "foundations-title", "Mathematical Foundations")),
      div(
        style = "border-top: 4px solid #F6F7F8;",
        class = "foundations-content",
        p(
          style = "color: #F6F7F8",
          class = "body-text",
          "Both the ", span("Gauss-Seidel", class = "highlighted-text"),
          " and ", span("Jacobi", class = "highlighted-text"),
          " methods are based on transforming a system of linear equations into iterative formulas. A system written in matrix form as Ax = b is rearranged so that each variable can be repeatedly approximated until convergence is achieved. These methods rely heavily on concepts such as matrix decomposition, diagonal dominance, convergence criteria, and iterative approximation."
        )
      )
    )
  )
}

#' Create the conditions section.
create_conditions_section <- function() {
  div(
    id = "conditions",
    div(
      class = "conditions-content",
      div(
        class = "conditions-subcontent",
        div(class = "conditions-title", "Conditions"),
        p(
          class = "body-text",
          "Both the ", span("Gauss-Seidel", class = "highlighted-text"),
          " and ", span("Jacobi", class = "highlighted-text"),
          " methods are based on transforming a system of linear equations into iterative formulas. A system written in matrix form as Ax = b is rearranged so that each variable can be repeatedly approximated until convergence is achieved. These methods rely heavily on concepts such as matrix decomposition, diagonal dominance, convergence criteria, and iterative approximation."
        )
      ),
      div(
        class = "conditions-subcontent",
        div(class = "conditions-subtitle", "Convergence"),
        p(
          class = "body-text",
          "A method is said to converge when successive approximations become increasingly close to the exact solution. For both Jacobi and Gauss-Seidel methods, convergence is commonly guaranteed when the coefficient matrix is ",
          span("strictly diagonally dominant", class = "highlighted-text"),
          " or symmetric positive definite."
        )
      ),
      div(
        class = "conditions-subcontent",
        div(class = "conditions-subtitle", "Divergence"),
        p(
          class = "body-text",
          "Divergence occurs when the iterative approximations move away from the actual solution instead of approaching it. This commonly happens when the matrix does not satisfy convergence conditions or when numerical instability affects the iteration process."
        )
      )
    ),
    div(
      style = "background: #183D5E; border-radius: 20px; margin: 60px 50px; margin-left: 0;",
      class = "conditions-content",
      div(style = "color: #F6F7F8;", class = "conditions-subtitle", "Convergence"),
      p(
        style = "color: #F6F7F8;",
        class = "body-text",
        "In practice, the ", span("Gauss-Seidel Method", class = "highlighted-text"),
        " often converges faster because it immediately uses updated values during computation. The ",
        span("Jacobi Method", class = "highlighted-text"),
        " is easier to parallelize because all updates depend only on values from the previous iteration. Choosing the appropriate method depends on the structure of the matrix and the computational requirements."
      )
    )
  )
}

#' Create one collapsible block for the Examples section.
create_example_collapse <- function(id, title, output_id) {
  div(
    style = "display: flex; flex-direction: column",
    div(
      class = "collapse-title",
      onclick = sprintf("toggleCollapse(this, '%s')", id),
      div(class = "title-text", title),
      img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")
    ),
    div(id = id, class = "collapse-content", uiOutput(output_id))
  )
}

#' Create the examples section.
create_examples_section <- function() {
  div(
    id = "examples",
    class = "section",
    div(
      class = "example-section-background",
      div(class = "example-section-box"),
      div(
        class = "example-section-inner",
        div(
          class = "nav-frame",
          div(class = "example-method-label", "Jacobi Method Examples"),
          div(
            class = "nav-controls",
            uiOutput("leftArrowUI"),
            div(class = "page-counter", textOutput("exampleCounter", inline = TRUE)),
            uiOutput("rightArrowUI")
          )
        ),
        h2(class = "example-title", "Example Problems"),
        div(
          style = "display: flex; flex-direction: column",
          create_example_collapse("algo", "Step-by-step Algorithm", "algoContent"),
          create_example_collapse("system", "Given System", "systemContent"),
          create_example_collapse("guess", "Initial Guess", "guessContent"),
          create_example_collapse("iter", "Iterations", "iterContent"),
          create_example_collapse("conv", "Convergence", "convContent"),
          create_example_collapse("error", "Error", "errorContent")
        )
      ),
      div(class = "example-section-box")
    )
  )
}

#' Create the calculator input panel.
create_calculator_input_panel <- function() {
  div(
    class = "calculator-panel",
    div(class = "calculator-panel-header", h2(class = "panel-title", "Jacobi Calculator")),
    div(
      class = "calculator-panel-body",
      div(
        class = "calc-field-group",
        div(
          class = "calc-inline-row",
          span(class = "calc-inline-label", "Number of Variables ="),
          div(
            class = "calc-mini-input",
            numericInput("calc_n", label = NULL, value = 3, min = 2, max = 6, step = 1)
          )
        )
      ),
      div(
        class = "calc-field-group",
        span(class = "calc-box-label", "System of Linear Equations"),
        div(
          class = "equation-textarea-wrap",
          textAreaInput(
            "calc_equations",
            label = NULL,
            value = "10x + 2y - 3z = 10\n-x + 5y - 3z = 4\n2x + 0y + 3z = 2",
            placeholder = "Example:\n-2x + 2y - 3z = 0\n-x + y - 3z = 0\n2x + 0y - z = 0",
            rows = 5,
            width = "100%"
          )
        )
      ),
      div(
        class = "calc-field-group",
        div(
          class = "calc-inline-row",
          span(class = "calc-inline-label", "Initial Guess ="),
          div(
            class = "calc-text-input",
            textInput("calc_initial_guess", label = NULL, value = "0, 0, 0", placeholder = "Example: 0, 0, 0")
          )
        )
      ),
      div(
        class = "calc-field-group",
        div(
          class = "calc-inline-row",
          span(class = "calc-inline-label", "Tolerance ="),
          div(class = "calc-mini-input", numericInput("calc_tolerance", label = NULL, value = 0.001, min = 0, step = 0.001))
        )
      ),
      div(
        class = "calc-field-group",
        div(
          class = "calc-inline-row",
          span(class = "calc-inline-label", "Max Iterations ="),
          div(class = "calc-mini-input", numericInput("calc_max_iter", label = NULL, value = 500, min = 1, step = 1))
        )
      ),
      actionButton("calc_submit", "Calculate", class = "calc-submit-btn"),
      div(
        class = "calc-help-text",
        "Input a square matrix A, vector b, tolerance, initial guess, and maximum iterations before clicking Calculate."
      )
    )
  )
}

#' Create the calculator results panel.
create_calculator_results_panel <- function() {
  div(
    class = "solutions-panel",
    div(
      class = "solutions-panel-header",
      h2(class = "solutions-title", "Results"),
      uiOutput("calculatorTabs")
    ),
    div(class = "solutions-panel-body", uiOutput("calculatorSolutionUI"))
  )
}

#' Create the complete calculator section.
create_calculator_section <- function() {
  div(
    id = "calculator",
    class = "section",
    div(
      class = "calculator-shell",
      create_calculator_input_panel(),
      create_calculator_results_panel()
    )
  )
}

#' JavaScript used for collapsible example panels.
create_app_scripts <- function() {
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
}


# ==============================================================================
# 5. Shiny User Interface
# ===============================================================================

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    withMathJax()
  ),
  create_navbar(),
  create_landing_section(),
  create_definitions_section(),
  create_foundations_section(),
  create_conditions_section(),
  create_examples_section(),
  create_calculator_section(),
  create_app_scripts()
)


# ==============================================================================
# 6. Server Helper Functions
# ===============================================================================

#' Reset all calculator-related reactive values.
#'
#' This prevents old results or errors from carrying over to a new calculation.
reset_calculator_state <- function(state) {
  state$calculatorTab <- "lud"
  state$calcResult <- NULL
  state$calcError <- NULL
  state$dominanceMessage <- NULL
  state$A <- NULL
  state$b <- NULL
  state$x0 <- NULL
  state$D <- NULL
  state$L <- NULL
  state$U <- NULL
}

#' Read and validate calculator inputs.
#'
#' This returns a list with parsed A, b, x0, tolerance, maximum iterations, and
#' validation errors. Errors are stored in a named list so they can be displayed
#' cleanly in the UI.
read_calculator_inputs <- function(input) {
  errors <- list()
  
  n <- as.integer(input$calc_n)
  tol <- input$calc_tolerance
  max_iter <- input$calc_max_iter
  equation_text <- trimws(input$calc_equations)
  guess_text <- trimws(input$calc_initial_guess)
  
  A <- NULL
  b <- NULL
  x0 <- NULL
  
  if (is.null(n) || is.na(n) || n < 2) {
    errors$size <- "Number of variables must be at least 2."
  }
  
  if (equation_text == "") {
    errors$equations <- "Please enter the system of equations."
  }
  
  if (length(errors) == 0) {
    parsed_system <- tryCatch(
      parse_equation_system(equation_text, n),
      error = function(e) {
        errors$equations <<- e$message
        NULL
      }
    )
    
    if (!is.null(parsed_system)) {
      A <- parsed_system$A
      b <- parsed_system$b
    }
    
    x0 <- tryCatch(
      parse_initial_guess(guess_text, n),
      error = function(e) {
        errors$guess <<- e$message
        NULL
      }
    )
  }
  
  # General validation checks after parsing.
  if (!is.null(A)) {
    if (nrow(A) != ncol(A)) {
      errors$A <- "Matrix A must be square."
    }
    
    if (any(is.na(A))) {
      errors$A <- "Matrix A contains invalid numbers."
    }
  }
  
  if (!is.null(A) && !is.null(b)) {
    if (length(b) != nrow(A)) {
      errors$b <- "Vector b must match number of rows in A."
    }
    
    if (any(is.na(b))) {
      errors$b <- "Vector b contains invalid numbers."
    }
  }
  
  if (!is.null(A) && !is.null(x0) && length(x0) != nrow(A)) {
    errors$guess <- "Initial guess must match size of A."
  }
  
  if (!is.null(tol) && tol <= 0) {
    errors$tolerance <- "Tolerance must be > 0."
  }
  
  if (!is.null(max_iter) && max_iter <= 0) {
    errors$iter <- "Max iterations must be > 0."
  }
  
  list(
    n = n,
    A = A,
    b = b,
    x0 = x0,
    tol = tol,
    max_iter = as.integer(max_iter),
    errors = errors
  )
}

#' Create a standard placeholder message.
placeholder_ui <- function(title, subtitle) {
  div(
    class = "calc-placeholder",
    style = "text-align: center; padding: 60px 20px; color: #64748b;",
    p(title),
    p(subtitle)
  )
}

#' Create the convergence status box in the Solution tab.
convergence_status_ui <- function(converged) {
  if (converged) {
    box_style <- "margin-top: 16px; padding: 12px; border-radius: 8px; background: #d1fae5; border: 1px solid #10b981;"
    text_style <- "margin: 0; font-weight: 600; color: #065f46;"
    message <- "✓ Converged successfully"
  } else {
    box_style <- "margin-top: 16px; padding: 12px; border-radius: 8px; background: #fee2e2; border: 1px solid #ef4444;"
    text_style <- "margin: 0; font-weight: 600; color: #991b1b;"
    message <- "⚠ Maximum iterations reached without convergence"
  }
  
  div(style = box_style, p(style = text_style, message))
}

#' Create a single iteration card for the calculator's Iterations tab.
iteration_card_ui <- function(iter_data) {
  k <- iter_data$k
  
  D_inv_latex <- matrix_to_latex(iter_data$D_inv_computed)
  b_latex <- vector_to_latex(iter_data$b)
  L_plus_U_latex <- matrix_to_latex(iter_data$L_plus_U)
  x_old_latex <- vector_to_latex(iter_data$x_old)
  x_new_latex <- vector_to_latex(iter_data$x)
  
  formula_latex <- paste0(
    "$$\\mathbf{x}^{(", k, ")} = ",
    "\\mathbf{D}^{-1} ",
    "\\left( \\mathbf{b} - (\\mathbf{L} + \\mathbf{U}) ",
    "\\mathbf{x}^{(", k - 1, ")} \\right)$$"
  )
  
  computation_latex <- paste0(
    "$$\\mathbf{x}^{(", k, ")} = ", D_inv_latex,
    " \\left( ", b_latex, " - ", L_plus_U_latex, " ", x_old_latex,
    " \\right) = ", x_new_latex, "$$"
  )
  
  div(
    class = "iteration-card",
    style = "border: 1px solid #e5e7eb; border-radius: 12px; padding: 20px; margin-bottom: 16px; background: white;",
    h4(paste("Iteration", k), style = "color: #183D5E; margin-top: 0;"),
    div(
      class = "formula-display",
      withMathJax(HTML(formula_latex)),
      withMathJax(HTML(computation_latex))
    ),
    div(
      style = "margin-top: 12px; padding: 10px; background: #f8fafc; border-radius: 8px;",
      p(style = "margin: 0; color: #334155;", strong("Error: "), sprintf("%.8f", iter_data$error))
    )
  )
}

#' Render the calculator Iterations tab.
iterations_tab_ui <- function(res) {
  if (is.null(res)) {
    return(placeholder_ui(
      "No iteration data available yet.",
      "Enter your matrix and vector values, then click Calculate."
    ))
  }
  
  iteration_displays <- lapply(2:length(res$iterations_detail), function(idx) {
    iteration_card_ui(res$iterations_detail[[idx]])
  })
  
  tagList(
    h3(class = "solution-heading", "Jacobi Iteration Steps"),
    div(style = "max-height: 600px; overflow-y: auto; padding-right: 10px;", iteration_displays)
  )
}

#' Render the calculator final Solution tab.
solution_tab_ui <- function(res) {
  if (is.null(res)) {
    return(placeholder_ui(
      "No solution data available yet.",
      "Enter your matrix and vector values, then click Calculate."
    ))
  }
  
  tagList(
    div(
      class = "solution-stack",
      h3(class = "solution-heading", "Final Solution"),
      div(
        class = "solutions-card",
        h4("Solution Vector", style = "color: #183D5E; margin-top: 0;"),
        tableOutput("solutionTable"),
        hr(style = "border-top: 1px solid #e5e7eb; margin: 20px 0;"),
        div(
          style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 16px;",
          div(
            p(style = "margin: 0; color: #64748b; font-size: 14px;", "Iterations Used"),
            p(style = "margin: 4px 0 0 0; color: #183D5E; font-size: 24px; font-weight: 600;", res$iterations)
          ),
          div(
            p(style = "margin: 0; color: #64748b; font-size: 14px;", "Final Error"),
            p(style = "margin: 4px 0 0 0; color: #183D5E; font-size: 24px; font-weight: 600;", sprintf("%.8f", res$final_error))
          )
        ),
        convergence_status_ui(res$converged)
      )
    )
  )
}

#' Render the calculator LUD and Dominance tab.
lud_tab_ui <- function(state) {
  if (is.null(state$D)) {
    return(placeholder_ui(
      "No matrix decomposition available yet.",
      "Enter your matrix and vector values, then click Calculate."
    ))
  }
  
  A_latex <- matrix_to_latex(state$A)
  L_latex <- matrix_to_latex(state$L)
  U_latex <- matrix_to_latex(state$U)
  D_latex <- matrix_to_latex(state$D)
  b_latex <- vector_to_latex(state$b)
  x0_latex <- vector_to_latex(state$x0)
  
  tagList(
    div(
      class = "solution-stack",
      h3(class = "solution-heading", "Matrix Decomposition"),
      if (!is.null(state$dominanceMessage)) {
        div(
          class = "solution-note",
          style = "background: #dbeafe; border: 1px solid #3b82f6; color: #1e40af;",
          p(style = "margin: 0;", state$dominanceMessage)
        )
      },
      div(
        class = "latex-solution-card",
        h4("Given System", style = "margin-top: 0; color: #183D5E;"),
        withMathJax(HTML(paste0(
          "$$\\mathbf{A} = ", A_latex, ",",
          "\\mathbf{b} = ", b_latex, ",",
          "\\mathbf{x}^{(0)} = ", x0_latex, "$$"
        )))
      ),
      div(
        class = "latex-solution-card",
        h4("Jacobi Matrix Splitting", style = "margin-top: 0; color: #183D5E;"),
        withMathJax(HTML(paste0(
          "$$\\mathbf{A} = \\mathbf{L} + \\mathbf{D} + \\mathbf{U}$$",
          "$$\\mathbf{L} = ", L_latex, ",",
          "\\mathbf{D} = ", D_latex, ",",
          "\\mathbf{U} = ", U_latex, "$$"
        )))
      ),
      div(
        class = "latex-solution-card",
        h4("Jacobi Formula", style = "margin-top: 0; color: #183D5E;"),
        withMathJax(HTML(
          "$$\\mathbf{x}^{(k+1)} = \\mathbf{D}^{-1}\\left(\\mathbf{b} - (\\mathbf{L} + \\mathbf{U})\\mathbf{x}^{(k)}\\right)$$"
        )),
        p("The Jacobi method separates A into its lower, diagonal, and upper parts, then updates x using only values from the previous iteration.")
      )
    )
  )
}

# ==============================================================================
# 7. Shiny Server
# ===============================================================================

server <- function(input, output, session) {
  example_bank <- get_example_bank()
  
  state <- reactiveValues(
    page = 1,
    calculatorTab = "lud",
    calcResult = NULL,
    calcError = NULL,
    dominanceMessage = NULL,
    A = NULL,
    b = NULL,
    x0 = NULL,
    D = NULL,
    L = NULL,
    U = NULL
  )
  
  # ---------------------------------------------------------------------------
  # Example section navigation
  # ---------------------------------------------------------------------------
  
  observeEvent(input$page_prev, {
    state$page <- if (state$page <= 1) length(example_bank) else state$page - 1
    session$sendCustomMessage("resetExampleCollapses", list())
  }, ignoreInit = TRUE)
  
  observeEvent(input$page_next, {
    state$page <- if (state$page >= length(example_bank)) 1 else state$page + 1
    session$sendCustomMessage("resetExampleCollapses", list())
  }, ignoreInit = TRUE)
  
  selected_example <- reactive({
    example_bank[[state$page]]
  })
  
  output$exampleCounter <- renderText({
    paste0(state$page, " / ", length(example_bank))
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
      class = if (state$page < length(example_bank)) "nav-action-btn" else "nav-action-btn disabled",
      onclick = if (state$page < length(example_bank)) "Shiny.setInputValue('page_next', Date.now(), {priority: 'event'})" else NULL,
      disabled = if (state$page >= length(example_bank)) NA else NULL,
      img(src = "assets/right-arrow-blue.svg", class = "arrow", alt = "Next page")
    )
  })
  
  # ---------------------------------------------------------------------------
  # Example section content
  # ---------------------------------------------------------------------------
  
  output$algoContent <- renderUI({
    tagList(
      div(class = "content-grid", div(class = "content-codebox", HTML(selected_example()$algo)))
    )
  })
  
  output$systemContent <- renderUI({
    ex <- selected_example()
    
    tagList(
      div(
        class = "content-grid",
        div(
          class = "content-pair",
          div(class = "content-value", div(class = "content-label", "Equation 1"), tags$p(ex$system[1], style = "margin: 0;")),
          div(class = "content-value", div(class = "content-label", "Equation 2"), tags$p(ex$system[2], style = "margin: 0;"))
        ),
        div(
          class = "content-note",
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
      div(
        class = "content-grid",
        div(
          class = "content-pair",
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
      div(
        class = "content-grid",
        numericInput("n_iter", "Number of iterations:", value = ex$iter, min = 1, max = 100),
        div(class = "content-note", "Define how many times the algorithm will be applied.")
      )
    )
  })
  
  output$convContent <- renderUI({
    ex <- selected_example()
    
    tagList(
      div(
        class = "content-grid",
        div(class = "content-label", "Convergence Plot"),
        div(class = "content-plotbox", plotOutput("convergencePlot", height = "250px")),
        div(class = "content-note", ex$conv)
      )
    )
  })
  
  output$errorContent <- renderUI({
    ex <- selected_example()
    
    tagList(
      div(
        class = "content-grid",
        div(class = "content-label", "Approximation Error"),
        div(class = "content-plotbox", plotOutput("errorPlot", height = "250px")),
        div(class = "content-tablebox", tableOutput("iterHistory")),
        div(class = "content-note", ex$err)
      )
    )
  })
  
  example_result <- reactive({
    ex <- selected_example()
    
    x1 <- input$x1
    x2 <- input$x2
    niter <- input$n_iter
    
    if (is.null(x1)) x1 <- ex$guess[1]
    if (is.null(x2)) x2 <- ex$guess[2]
    if (is.null(niter)) niter <- ex$iter
    
    run_jacobi_example_iterations(
      A = ex$A,
      b = ex$b,
      x0 = c(x1, x2),
      max_iter = niter
    )
  })
  
  output$convergencePlot <- renderPlot({
    res <- example_result()
    history <- as.matrix(res$history)
    iterations <- 0:(nrow(history) - 1)
    
    matplot(
      iterations,
      history,
      type = "b",
      pch = 19,
      lty = 1,
      col = c("steelblue", "firebrick"),
      xlab = "Iteration",
      ylab = "Value",
      main = "Jacobi Iteration Convergence"
    )
    grid()
  })
  
  output$errorPlot <- renderPlot({
    res <- example_result()
    errors <- res$errors[-1]
    iterations <- seq_along(errors)
    
    plot(
      iterations,
      errors,
      type = "b",
      pch = 19,
      col = "darkred",
      xlab = "Iteration",
      ylab = "Error",
      main = "Approximation Error"
    )
    grid()
  })
  
  output$iterHistory <- renderTable({
    res <- example_result()
    history <- res$history
    
    data.frame(
      Iteration = 0:(nrow(history) - 1),
      x1 = round(history$x1, 6),
      x2 = round(history$x2, 6),
      Error = round(res$errors, 8)
    )
  })
  
  # ---------------------------------------------------------------------------
  # Calculator interactions
  # ---------------------------------------------------------------------------
  
  observeEvent(input$calculatorTab, {
    state$calculatorTab <- input$calculatorTab
  }, ignoreInit = TRUE)
  
  observeEvent(input$calc_submit, {
    reset_calculator_state(state)
    parsed <- read_calculator_inputs(input)
    
    if (length(parsed$errors) > 0) {
      state$calcError <- parsed$errors
      return()
    }
    
    A <- parsed$A
    b <- parsed$b
    x0 <- parsed$x0
    
    # Row rearrangement is attempted so that the Jacobi method has better
    # convergence behavior when a diagonally dominant ordering exists.
    dominant_order <- find_dominant_order(A)
    
    if (is.null(dominant_order)) {
      state$calcError <- list(
        A = "Matrix A is not diagonally dominant, and no row arrangement can make it diagonally dominant. The Jacobi method may not converge."
      )
      return()
    }
    
    if (!identical(as.integer(dominant_order), seq_len(nrow(A)))) {
      A <- A[dominant_order, , drop = FALSE]
      b <- b[dominant_order]
      state$dominanceMessage <- paste(
        "Matrix A was rearranged into diagonally dominant form using row order:",
        paste(dominant_order, collapse = ", ")
      )
    } else {
      state$dominanceMessage <- "Matrix A is already diagonally dominant."
    }
    
    decomposition <- extract_LDU(A)
    state$A <- A
    state$b <- b
    state$x0 <- x0
    state$D <- decomposition$D
    state$L <- decomposition$L
    state$U <- decomposition$U
    
    if (any(diag(state$D) == 0)) {
      state$calcError <- list(D = "Zero found in the diagonal. Jacobi method cannot proceed.")
      return()
    }
    
    calc_result <- tryCatch(
      jacobi_detailed(
        A = A,
        b = b,
        x0 = x0,
        tol = parsed$tol,
        max_iter = parsed$max_iter
      ),
      error = function(e) {
        state$calcError <- list(calculation = paste("Calculation error:", e$message))
        NULL
      }
    )
    
    if (is.null(calc_result)) {
      return()
    }
    
    state$calcError <- NULL
    state$calcResult <- calc_result
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
    
    div(
      class = "solutions-tabs",
      tab_button("lud", "LUD & Dominance"),
      tab_button("iterations", "Iterations"),
      tab_button("solutions", "Solution")
    )
  })
  
  output$calculatorSolutionUI <- renderUI({
    if (!is.null(state$calcError)) {
      return(
        div(
          class = "calc-error-block",
          lapply(names(state$calcError), function(field) {
            div(
              class = "calc-error-message",
              strong(paste0(toupper(substring(field, 1, 1)), substring(field, 2), ": ")),
              state$calcError[[field]]
            )
          })
        )
      )
    }
    
    if (state$calculatorTab == "iterations") {
      return(iterations_tab_ui(state$calcResult))
    }
    
    if (state$calculatorTab == "solutions") {
      return(solution_tab_ui(state$calcResult))
    }
    
    lud_tab_ui(state)
  })
  
  output$solutionTable <- renderTable({
    req(state$calcResult)
    
    n <- length(state$calcResult$solution)
    
    data.frame(
      Variable = paste0("x", to_subscript(seq_len(n))),
      Value = round(state$calcResult$solution, 8)
    )
  }, align = "c")
}

# ==============================================================================
# 8. Run Application
# ===============================================================================

shinyApp(ui = ui, server = server)