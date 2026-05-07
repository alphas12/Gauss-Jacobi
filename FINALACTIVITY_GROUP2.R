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
    # ChunkFive Font
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=ChunkFive&display=swap",
      rel = "stylesheet"
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
        padding: 120px 0;
        margin-top: 0;
        scroll-margin-top: 120px;
      }
    
    /* Example Section */
    .example-section-background {
      background: #F6F7F8;
      border-radius: 12px;
      overflow: hidden;
      box-shadow: 0 4px 14px rgba(0, 0, 0, 0.12);
      border: 1px solid #183D5E;
    }

    .example-section-box {
      height: 120px;
      background: #183D5E;
    }

    .example-section-inner {
      display: flex;
      padding: 25px 60px 40px;
      flex-direction: column;
      align-items: stretch;
      gap: 24px;
      align-self: stretch;
    }

    .example-header {
      display: flex;
      justify-content: space-between;
      align-items: flex-end;
      gap: 24px;
      flex-wrap: wrap;
    }

    .nav-frame {
      display: flex;
      justify-content: space-between;
      align-items: center;
      align-self: stretch;
      padding: 0;
      width: auto;
      min-width: 360px;
      gap: 20px;
    }

    .nav-buttons {
      display: flex;
      gap: 13px;
      align-items: center;
    }

    .nav-action-btn {
      border: none;
      background: transparent;
      padding: 0;
      cursor: pointer;
      display: flex;
      align-items: center;
      justify-content: center;
    }

    .nav-action-btn.disabled {
      opacity: 0.3;
      cursor: not-allowed;
      pointer-events: none;
    }

    .method-btn {
      display: flex;
      padding: 15px 25px;
      justify-content: center;
      align-items: center;
      gap: 10px;
        border-radius: 10px;
        border: 3px solid #2EC4B6;
        background: #F6F7F8;
        box-shadow: 0 4px 10.9px 0 rgba(0, 0, 0, 0.12);
      color: #2EC4B6;
      font-family: Inter;
      font-size: 14px;
      font-weight: 500;
      cursor: pointer;
      transition: all 0.3s ease;
    }

    .method-btn:hover {
      background: #2EC4B6;
      color: #F6F7F8;
    }

    .nav-controls {
      display: flex;
      justify-content: center;
      align-items: center;
      gap: 20px;
    }

    .nav-controls .page-counter {
      min-width: 44px;
    }

    .nav-controls .arrow {
      width: 11px;
      height: 22px;
      cursor: pointer;
    }

    .page-counter {
      color: #183D5E;
      text-align: center;
      font-family: Inter;
      font-size: 16px;
      font-style: normal;
      font-weight: 500;
      line-height: normal;
      letter-spacing: 2.08px;
    }

    .example-subtitle {
      color: #183D5E;
      font-family: Inter, sans-serif;
      font-size: 16px;
      font-weight: 600;
      margin-top: -10px;
      align-self: flex-start;
    }

    .example-title {
      color: #164670;
      text-shadow: 0 4px 4px rgba(0, 0, 0, 0.16);
      font-family: ChunkFive;
      font-size: 40px;
      font-style: normal;
      font-weight: 400px;
      line-height: normal;
      margin: 0;
    }
    
    /* Styles for Collapsible (Examples section only) */
    #examples .collapse-title {
      color: #F6F7F8;
      font-family: Inter;
      font-size: 16px;
      font-style: normal;
      font-weight: 500;
      line-height: 1.4;
      background-color: #2E5090;
      padding: 18px 18px;
      margin: 0;
      border-radius: 10px;
      cursor: pointer;
      user-select: none;
      display: flex;
      justify-content: space-between;
      align-items: center;
      transition: background-color 0.2s ease, border-radius 0.2s ease, margin-bottom 0.2s ease;
    }

    #examples .collapse-title:hover {
      background-color: #1a3a6b;
    }

    #examples .collapse-title .title-text {
      font-weight: 500;
    }

    #examples .collapse-content {
      display: none;
      padding: 18px;
      background-color: #fff;
      border: 1px solid #e5e7eb;
      border-top: none;
      border-radius: 0 0 10px 10px;
      margin-top: 0;
      line-height: 1.65;
      color: #334155;
      font-size: 14px;
      box-shadow: 0 8px 18px rgba(15, 23, 42, 0.04);
    }
    #examples .collapse-content.show {
      display: block;
    }

    #examples .collapse-title .arrow {
      width: 20px;
      height: 20px;
      transform: rotate(-90deg);
      transition: transform 0.2s ease;
      flex-shrink: 0;
    }

    #examples .collapse-title.open .arrow {
      transform: rotate(0deg);
    }

    #examples .collapse-title.open {
      border-radius: 10px 10px 0 0;
      margin-bottom: 0;
    }

    #examples .collapse-content p {
      margin-bottom: 0.75rem;
    }

    #examples .collapse-content h5 {
      margin: 0 0 0.75rem 0;
      color: #f1f5f9;
      font-size: 15px;
      font-weight: 600;
    }

    #examples .collapse-content code {
      display: inline-block;
      padding: 0.15rem 0.4rem;
      border-radius: 0.375rem;
      background: #f1f5f9;
      color: #0f172a;
      font-size: 0.95em;
    }

    #examples .collapse-content ul,
    #examples .collapse-content ol {
      margin: 0.5rem 0 0.75rem 1.25rem;
      padding: 0;
    }

    #examples .collapse-card {
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
    }

    #examples .content-grid {
      display: grid;
      gap: 0.75rem;
    }

    #examples .content-pair {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 0.75rem;
    }

    #examples .content-label {
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
      font-size: 12px;
      font-weight: 700;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: #475569;
    }

    #examples .content-value,
    #examples .content-note,
    #examples .content-plotbox,
    #examples .content-tablebox {
      border: 1px solid #e5e7eb;
      border-radius: 12px;
      background: #fff;
      padding: 14px 16px;
    }

    #examples .content-note {
      color: #334155;
      line-height: 1.65;
      background: #f8fafc;
    }

    #examples .content-plotbox {
      padding: 12px;
      box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.5);
    }

    #examples .content-tablebox {
      width: 100%;
      overflow-x: auto;
      padding: 0;
    }

    #examples .content-tablebox table {
      width: 100%;
      border-collapse: collapse;
    }

    #examples .content-tablebox table.table,
    #examples .content-tablebox table.dataTable {
      width: 100% !important;
    }

    #examples .content-tablebox .table {
      width: 100% !important;
    }

    #examples .content-tablebox .table-responsive {
      width: 100%;
    }

    #examples .content-tablebox th,
    #examples .content-tablebox td {
      padding: 10px 12px;
      border-bottom: 1px solid #e2e8f0;
      text-align: left;
      font-size: 13px;
    }

    #examples .content-tablebox th {
      color: #0f172a;
      font-weight: 700;
      background: #f8fafc;
    }

    #examples .content-codebox {
      border: 1px solid #0f172a;
      border-radius: 12px;
      background: #0f172a;
      color: #e2e8f0;
      padding: 14px 16px;
      overflow-x: auto;
    }

    #examples .content-codebox code,
    #examples .content-codebox pre {
      margin: 0;
      white-space: pre-wrap;
      word-break: break-word;
      background: transparent;
      color: inherit;
      padding: 0;
    }

    #examples .content-stack {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
    }

    /* Calculator Section */
    #calculator .calculator-shell {
      display: grid;
      grid-template-columns: minmax(320px, 0.95fr) minmax(0, 1.55fr);
      gap: 12px;
      align-items: stretch;
    }

    #calculator .calculator-panel,
    #calculator .solutions-panel {
      border: 2px solid #1d466f;
      border-radius: 18px;
      overflow: hidden;
      background: #f6f7f8;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.14);
    }

    #calculator .calculator-panel-header {
      background: #183D5E;
      padding: 18px 20px;
      text-align: center;
    }

    #calculator .calculator-panel-header .panel-title {
      color: #F6F7F8;
      font-family: ChunkFive;
      font-size: 32px;
      font-weight: 400;
      margin: 0;
    }

    #calculator .calculator-panel-body {
      padding: 18px 18px 16px;
      display: flex;
      flex-direction: column;
      gap: 16px;
    }

    #calculator .calculator-methods,
    #calculator .solutions-tabs {
      display: flex;
      gap: 10px;
      flex-wrap: wrap;
    }

    #calculator .calc-tab-btn,
    #calculator .calc-method-btn {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      padding: 10px 18px;
      border-radius: 8px;
      border: 3px solid #2EC4B6;
      background: #F6F7F8;
      color: #2EC4B6;
      font-family: Inter;
      font-size: 13px;
      font-weight: 500;
      cursor: pointer;
      transition: background-color 0.2s ease, color 0.2s ease, transform 0.2s ease, box-shadow 0.2s ease;
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.10);
    }

    #calculator .calc-method-btn.active,
    #calculator .calc-tab-btn.active {
      background: #2EC4B6;
      color: #F6F7F8;
    }

    #calculator .calc-method-btn:hover,
    #calculator .calc-tab-btn:hover {
      background: #2EC4B6;
      color: #F6F7F8;
      transform: translateY(-1px);
    }

    #calculator .calc-submit-btn {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      align-self: flex-start;
      min-width: 120px;
      padding: 11px 20px;
      border-radius: 10px;
      border: 3px solid #2EC4B6;
      background: #2EC4B6;
      color: #F6F7F8;
      font-family: Inter;
      font-size: 14px;
      font-weight: 600;
      cursor: pointer;
      transition: background-color 0.2s ease, transform 0.2s ease, box-shadow 0.2s ease;
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.10);
    }

    #calculator .calc-submit-btn:hover {
      background: #1fa99d;
      border-color: #1fa99d;
      transform: translateY(-1px);
    }

    #calculator .calc-help-text {
      color: #64748b;
      font-family: Inter;
      font-size: 13px;
      line-height: 1.5;
      margin-top: -4px;
    }

    #calculator .calc-method-btn:focus,
    #calculator .calc-tab-btn:focus {
      outline: none;
    }

    #calculator .calc-field-group {
      display: flex;
      flex-direction: column;
      gap: 10px;
    }

    #calculator .calc-inline-row {
      display: flex;
      align-items: center;
      gap: 10px;
      flex-wrap: wrap;
    }

    #calculator .calc-inline-label {
      color: #183D5E;
      font-family: Inter;
      font-size: 16px;
      font-weight: 600;
      white-space: nowrap;
    }

    #calculator .calc-mini-input {
      width: 78px;
    }

    #calculator .calc-mini-input input {
      text-align: center;
      border: 2px solid #234e79;
      border-radius: 8px;
      background: #fff;
      box-shadow: none;
    }

    #calculator .calc-box-label {
      color: #183D5E;
      font-family: Inter;
      font-size: 16px;
      font-weight: 600;
      margin: 0;
      line-height: 1;
      white-space: nowrap;
    }

    #calculator .calc-textarea textarea,
    #calculator .calc-text-input input {
      width: 100%;
      border: 2px solid #234e79;
      border-radius: 8px;
      background: #fff;
      color: #183D5E;
      font-family: Inter;
      box-shadow: none;
      transition: border-color 0.2s ease, box-shadow 0.2s ease;
    }

    #calculator .calc-textarea textarea {
      min-height: 96px;
      resize: vertical;
    }

    #calculator .calc-textarea textarea:focus,
    #calculator .calc-text-input input:focus,
    #calculator .calc-mini-input input:focus {
      border-color: #2EC4B6;
      box-shadow: 0 0 0 3px rgba(46, 196, 182, 0.18);
      outline: none;
    }

    #calculator .solutions-panel-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 16px;
      padding: 24px 28px 16px;
      flex-wrap: wrap;
    }

    #calculator .solutions-title {
      color: #183D5E;
      font-family: ChunkFive;
      font-size: 32px;
      font-weight: 400;
      margin: 0;
      text-shadow: 0 4px 4px rgba(0, 0, 0, 0.16);
    }

    #calculator .solutions-panel-body {
      padding: 8px 28px 26px;
      min-height: 360px;
    }

    #calculator .solution-stack {
      display: flex;
      flex-direction: column;
      gap: 18px;
    }

    #calculator .solution-heading {
      color: #183D5E;
      font-family: Inter;
      font-size: 22px;
      font-weight: 600;
      margin: 0;
    }

    #calculator .solution-note {
      color: #334155;
      font-family: Inter;
      font-size: 15px;
      line-height: 1.7;
      border: 1px solid #e5e7eb;
      border-radius: 14px;
      background: #fff;
      padding: 16px 18px;
    }

    #calculator .lu-diagram {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 18px;
      align-items: stretch;
    }

    #calculator .factor-card {
      background: #fff;
      border: 1px solid #dbe4ee;
      border-radius: 16px;
      padding: 14px;
      box-shadow: 0 6px 16px rgba(15, 23, 42, 0.05);
    }

    #calculator .factor-label {
      color: #183D5E;
      font-family: Inter;
      font-size: 18px;
      font-weight: 700;
      margin-bottom: 12px;
    }

    #calculator .matrix-view {
      position: relative;
      min-height: 220px;
      border-radius: 14px;
      background: linear-gradient(180deg, #fbfdff 0%, #f6f9fc 100%);
      border: 1px dashed #c7d3df;
    }

    #calculator .matrix-view::before,
    #calculator .matrix-view::after {
      content: '';
      position: absolute;
      top: 16px;
      bottom: 16px;
      width: 26px;
      border-top: 3px solid #234e79;
      border-bottom: 3px solid #234e79;
    }

    #calculator .matrix-view::before {
      left: 14px;
      border-left: 3px solid #234e79;
    }

    #calculator .matrix-view::after {
      right: 14px;
      border-right: 3px solid #234e79;
    }

    #calculator .matrix-placeholder-grid {
      position: absolute;
      inset: 18px 44px;
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      grid-template-rows: repeat(3, 1fr);
      gap: 12px;
      align-items: center;
      justify-items: center;
    }

    #calculator .matrix-placeholder-grid span {
      width: 100%;
      height: 20px;
      border-radius: 999px;
      background: rgba(36, 78, 121, 0.12);
    }

    #calculator .iteration-list {
      display: grid;
      gap: 12px;
    }

    #calculator .iteration-row {
      display: grid;
      grid-template-columns: 1fr auto;
      align-items: center;
      gap: 12px;
      border: 1px solid #e5e7eb;
      border-radius: 12px;
      background: #fff;
      padding: 14px 16px;
      color: #183D5E;
      font-family: Inter;
      font-size: 15px;
      font-weight: 500;
    }

    #calculator .iteration-row span:last-child {
      color: #2EC4B6;
      font-weight: 700;
    }

    #calculator .text-card {
      border: 1px solid #e5e7eb;
      border-radius: 14px;
      background: #fff;
      padding: 18px;
      color: #334155;
      line-height: 1.7;
    }

    #calculator .text-card strong {
      color: #183D5E;
    }

    @media (max-width: 980px) {
      #calculator .calculator-shell {
        grid-template-columns: 1fr;
      }

      #calculator .lu-diagram {
        grid-template-columns: 1fr;
      }
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
          
          # Step-by-step Algorithm
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'algo')",
              div(class = "title-text", "Step-by-step Algorithm"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "algo", class = "collapse-content", uiOutput("algoContent")),
          
          # Given System
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'system')",
              div(class = "title-text", "Given System"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "system", class = "collapse-content", uiOutput("systemContent")),
          
          # Initial Guess
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'guess')",
              div(class = "title-text", "Initial Guess"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "guess", class = "collapse-content", uiOutput("guessContent")),
          
          # Iterations
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'iter')",
              div(class = "title-text", "Iterations"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "iter", class = "collapse-content", uiOutput("iterContent")),
          
          # Convergence
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'conv')",
              div(class = "title-text", "Convergence"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "conv", class = "collapse-content", uiOutput("convContent")),
          
          # Error
            div(class = "collapse-title", onclick = "toggleCollapse(this, 'error')",
              div(class = "title-text", "Error"),
              img(src = "assets/down-arrow-white.svg", class = "arrow", alt = "Expand")),
            div(id = "error", class = "collapse-content", uiOutput("errorContent"))
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
            div(class = "calc-inline-row",
              span(class = "calc-inline-label", "Row x Column ="),
              div(class = "calc-mini-input", numericInput("calc_rows", label = NULL, value = 2, min = 1, max = 20)),
              span(class = "calc-inline-label", "x"),
              div(class = "calc-mini-input", numericInput("calc_cols", label = NULL, value = 2, min = 1, max = 20))
            )
          ),
          div(class = "calc-field-group",
            span(class = "calc-box-label", "A ="),
            div(class = "calc-textarea", textAreaInput("calc_matrixA", label = NULL, placeholder = "Enter matrix values, e.g. 3 1; 1 4", rows = 4))
          ),
          div(class = "calc-field-group",
            span(class = "calc-box-label", "b ="),
            div(class = "calc-textarea", textAreaInput("calc_vectorb", label = NULL, placeholder = "Enter vector values, e.g. 5, 6", rows = 4))
          ),
          div(class = "calc-field-group",
            div(class = "calc-inline-row",
              span(class = "calc-inline-label", "Initial Guess ="),
              div(class = "calc-text-input", textInput("calc_initial_guess", label = NULL, placeholder = "Enter initial guess, e.g. 0, 0"))
            )
          )
          ,actionButton("calc_submit", "Enter", class = "calc-submit-btn"),
          div(class = "calc-help-text", "Click Enter after filling the fields to update the solution panel.")
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
        content.style.display = 'block';
      } else {
        element.classList.remove('open');
        element.setAttribute('aria-expanded', 'false');
        content.style.display = 'none';
      }
    }

    Shiny.addCustomMessageHandler('resetExampleCollapses', function() {
      ['algo', 'system', 'guess', 'iter', 'conv', 'error'].forEach(function(id) {
        const content = document.getElementById(id);
        const trigger = content ? content.previousElementSibling : null;
        if (content) {
          content.classList.remove('show');
          content.style.display = 'none';
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

  state <- reactiveValues(method = "gauss", page = 1, calcMethod = "gauss", calculatorTab = "lu")

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
    state$calculatorTab <- "text"
  }, ignoreInit = TRUE)

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
      tab_button("lu", "LU"),
      tab_button("iterations", "Iterations"),
      tab_button("text", "Text")
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
    } else if (state$calculatorTab == "text") {
      tagList(
        div(class = "solution-stack",
          h3(class = "solution-heading", "Text"),
          div(class = "text-card",
            tags$p(tags$strong("Selected method:"), paste(state$calcMethod)),
            tags$p(tags$strong("Matrix A:"), "Enter values in row-by-row form."),
            tags$p(tags$strong("Vector b:"), "Enter the right-hand side values."),
            tags$p(tags$strong("Initial guess:"), "Use a comma-separated vector.")
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