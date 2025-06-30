# Load the required libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(fontawesome)
library(DT)

# Global variable for Levenshtein distance threshold
max_acceptable_Levenshtein_distance <- 2

# Enhanced game data loading with validation
loadGameData <- function(file_path) {
  tryCatch({
    # Simple approach: check if file exists, if not, try common locations
    if (!file.exists(file_path)) {
      # Try in the same directory as this script (for when sourced from RStudio)
      alt_path <- file.path(dirname(sys.frame(1)$ofile %||% getwd()), file_path)
      if (file.exists(alt_path)) {
        file_path <- alt_path
      } else {
        stop("File does not exist: ", file_path)
      }
    }
    
    data <- readLines(file_path, warn = FALSE)
    
    if (length(data) < 2) {
      stop("Invalid file format: insufficient data")
    }
    
    targets <- strsplit(x = data[1], split = ", ") |> unlist()
    clues <- data[2:length(data)]
    
    if (length(targets) == 0 || length(clues) == 0) {
      stop("Invalid file format: missing targets or clues")
    }
    
    list(targets = targets, clues = clues, target_player = targets[1])
  }, error = function(e) {
    warning("Error loading game data from ", file_path, ": ", e$message)
    # Fallback data with sample football player
    return(list(
      targets = c("Lionel Messi", "Messi"), 
      clues = c(
        "I am from Argentina",
        "I have won multiple Ballon d'Or awards",
        "I played for Barcelona for most of my career",
        "I won the 2022 FIFA World Cup"
      ),
      target_player = "Lionel Messi"
    ))
  })
}

# Initialize with default game data
game_data <- loadGameData("clues.txt")

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS and JS 
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Atkinson+Hyperlegible:400,700&display=swap"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
    tags$style(HTML("
      :root {
        --bg-color: #f8f9fa;
        --card-bg: white;
        --text-color: #343a40;
        --primary-color: #1a5a96;
        --secondary-bg: #f8f9fa;
        --border-color: #e9ecef;
        --shadow-color: rgba(0,0,0,0.08);
        --transition-speed: 0.3s;
      }
      
      [data-theme='dark'] {
        --bg-color: #212529;
        --card-bg: #343a40;
        --text-color: #f8f9fa;
        --primary-color: #4d94d3;
        --secondary-bg: #2b3035;
        --border-color: #495057;
        --shadow-color: rgba(0,0,0,0.2);
      }
      
      body {
        font-family: 'Atkinson Hyperlegible', sans-serif;
        background-color: var(--bg-color);
        color: var(--text-color);
        transition: background-color var(--transition-speed) ease;
      }
      
      .player-name {
        font-family: 'Arial', sans-serif;
        font-weight: 600;
      }
      
      .header {
        background: var(--primary-color);
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 6px;
        box-shadow: 0 2px 4px var(--shadow-color);
        transition: all var(--transition-speed) ease;
        position: relative; /* Added for positioning the toggle button */
      }
      
      .card {
        background-color: var(--card-bg);
        border-radius: 6px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px var(--shadow-color);
        transition: all var(--transition-speed) ease;
        border: 1px solid var(--border-color);
        animation: slideUpIn 0.4s ease-out;
      }
      
      @keyframes slideUpIn {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .card:hover {
        box-shadow: 0 3px 8px var(--shadow-color);
        transform: translateY(-2px);
      }
      
      .card-title {
        font-weight: 700;
        color: var(--primary-color);
        margin-bottom: 15px;
        border-bottom: 1px solid var(--border-color);
        padding-bottom: 10px;
        transition: color var(--transition-speed) ease;
      }
      
      .btn-primary {
        background-color: var(--primary-color);
        border-color: var(--primary-color);
        transition: all 0.2s ease;
      }
      
      .btn-primary:hover {
        background-color: #12477a;
        border-color: #12477a;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .btn-primary:active {
        transform: scale(0.97);
        transition: transform 0.1s;
      }
      
      .btn-secondary {
        background-color: #6c757d;
        border-color: #6c757d;
        transition: all 0.2s ease;
        color: white;
      }
      
      .btn-secondary:hover {
        background-color: #5a6268;
        border-color: #5a6268;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .btn-secondary:active {
        transform: scale(0.97);
        transition: transform 0.1s;
      }
      
      .game-mode-buttons {
        display: flex;
        justify-content: space-between;
        margin-bottom: 15px;
      }
      
      .game-mode-buttons .btn {
        flex: 1;
        margin: 0 5px;
      }
      
      .clue-box {
        background-color: var(--secondary-bg);
        border-left: 3px solid var(--primary-color);
        padding: 10px 15px;
        margin-bottom: 10px;
        border-radius: 3px;
        transition: all var(--transition-speed) ease;
        animation: slideInFromLeft 0.6s ease-out both;
        opacity: 0;
        transform: translateX(-20px);
      }
      
      @keyframes slideInFromLeft {
        to {
          opacity: 1;
          transform: translateX(0);
        }
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      @keyframes pulse {
        0%, 100% { transform: scale(1); }
        50% { transform: scale(1.02); }
      }
      
      @keyframes shake {
        0%, 100% { transform: translateX(0); }
        25% { transform: translateX(-3px); }
        75% { transform: translateX(3px); }
      }
      
      .clue-number {
        font-weight: 700;
        color: var(--primary-color);
        margin-right: 5px;
        transition: color var(--transition-speed) ease;
      }
      
      .score-display {
        font-size: 36px;
        font-weight: 700;
        color: var(--primary-color);
        text-align: center;
        transition: color var(--transition-speed) ease;
      }
      
      .guess-history {
        background-color: var(--secondary-bg);
        padding: 12px;
        border-radius: 4px;
        margin-top: 15px;
        border: 1px solid var(--border-color);
        transition: all var(--transition-speed) ease;
      }
      
      .guess-history-title {
        font-weight: 600;
        color: var(--text-color);
        margin-bottom: 8px;
        transition: color var(--transition-speed) ease;
      }
      
      .guess-item {
        display: inline-block;
        background-color: var(--secondary-bg);
        padding: 4px 8px;
        margin: 3px;
        border-radius: 4px;
        font-size: 0.9em;
        transition: all 0.2s ease;
        border: 1px solid var(--border-color);
        animation: popIn 0.3s ease-out;
        transform: scale(0.8);
        animation-fill-mode: forwards;
      }
      
      @keyframes popIn {
        to {
          transform: scale(1);
        }
      }
      
      .guess-item:hover {
        transform: translateY(-2px) scale(1.05);
        box-shadow: 0 4px 8px var(--shadow-color);
      }
      
      .guess-skip {
        background-color: var(--secondary-bg);
        color: #6c757d;
      }
      
      .result-message {
        padding: 10px;
        margin-top: 15px;
        border-radius: 3px;
        transition: all var(--transition-speed) ease;
        animation: slideInFromRight 0.4s ease-out;
        transform: translateX(10px);
        opacity: 0;
        animation-fill-mode: forwards;
      }
      
      @keyframes slideInFromRight {
        to {
          opacity: 1;
          transform: translateX(0);
        }
      }
      
      .result-success {
        background-color: #e5f0f9;
        color: #1a5a96;
        border: 1px solid #c9e0f3;
      }
      
      [data-theme='dark'] .result-success {
        background-color: #264653;
        color: #a8dadc;
        border: 1px solid #2a9d8f;
      }
      
      .result-error {
        background-color: #f7eeee;
        color: #866868;
        border: 1px solid #e7dbdb;
      }
      
      [data-theme='dark'] .result-error {
        background-color: #3d2b2b;
        color: #e08c8c;
        border: 1px solid #583535;
      }
      
      .result-neutral {
        background-color: #e9ecef;
        color: #495057;
        border: 1px solid #dee2e6;
      }
      
      [data-theme='dark'] .result-neutral {
        background-color: #343a40;
        color: #adb5bd;
        border: 1px solid #495057;
      }
      
      .footer {
        text-align: center;
        padding: 20px;
        margin-top: 30px;
        color: #6c757d;
        transition: color var(--transition-speed) ease;
      }
      
      .footer a {
        color: var(--primary-color);
        transition: color var(--transition-speed) ease;
      }
      
      #guess {
        border: 1px solid var(--border-color);
        border-radius: 4px;
        padding: 10px;
        font-size: 16px;
        background-color: var(--card-bg);
        color: var(--text-color);
        transition: all var(--transition-speed) ease;
      }
      
      #submit_guess {
        width: 100%;
        padding: 10px;
        font-weight: 600;
        margin-top: 10px;
        font-size: 16px;
        position: relative;
        overflow: hidden;
      }
      
      #submit_guess:disabled {
        opacity: 0.7;
        animation: pulse 1s infinite;
      }
      
      #submit_guess:not(:disabled):active {
        transform: scale(0.98);
      }
      
      .mode-toggle {
        position: absolute;
        top: 10px;
        right: 10px;
      }
      
      .mode-toggle .btn {
        border-radius: 50%;
        width: 40px;
        height: 40px;
        padding: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        background-color: rgba(255,255,255,0.2);
        color: white;
        border: 1px solid rgba(255,255,255,0.3);
        transition: all var(--transition-speed) ease;
      }
      
      .mode-toggle .btn:hover {
        transform: rotate(30deg);
        background-color: rgba(255,255,255,0.3);
      }
      
      /* Removed clues-card min-height */
      
      /* Game info formatting */
      .game-info {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
        font-size: 0.9em;
      }
      
      .game-info-text {
        color: #6c757d;
      }
      
      /* Mobile optimizations */
      @media (max-width: 768px) {
        .header {
          padding: 15px;
          margin-bottom: 15px;
        }
        
        .card {
          padding: 15px;
          margin-bottom: 15px;
        }
        
        .score-display {
          font-size: 28px;
        }
        
        #guess {
          font-size: 18px;
          padding: 12px;
        }
        
        #submit_guess {
          padding: 12px;
          font-size: 18px;
        }
        
        .clue-box {
          padding: 8px 12px;
        }
      }
      
      /* Accessibility improvements */
      @media (prefers-reduced-motion: reduce) {
        *, *::before, *::after {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
        }
      }
    "))
  ),
  
  # Header with mode toggle inside
  div(class = "header",
      div(class = "mode-toggle",
          actionButton("toggle_mode", "", icon = icon("moon"))
      ),
      h2("who am I?", style = "text-align: center; margin-bottom: 0;"),
      h5(icon("futbol"), " guess the football legend using as few clues as you can ", icon("futbol"), 
         style = "text-align: center; margin-top: 5px; font-weight: 400;")
  ),
  
  # Game mode selection buttons - responsive for mobile
  div(class = "container",
      div(class = "row game-mode-buttons",
          div(class = "col-12 col-md-6 mb-2 mb-md-0", # Added mobile-specific classes
              actionButton("play_latest", "latest clues", class = "btn-primary", width = "100%",
                           icon = icon("play"))
          ),
          div(class = "col-12 col-md-6", # Added mobile-specific classes
              actionButton("play_random", "archived clues", class = "btn-secondary", width = "100%",
                           icon = icon("random"))
          )
      )
  ),
  
  # Main content
  fluidRow(
    column(4,
           div(class = "card",
               h3(class = "card-title", "Make Your Guess"),
               textInput("guess", "", placeholder = "Enter player name..."),
               actionButton("submit_guess", "Submit Guess", class = "btn-primary"),
               div(id = "result-message"), # For success/failure messages
               div(class = "guess-history", id = "guess-history",
                   div(class = "guess-history-title", "Previous Guesses:"),
                   div(id = "guess-list")
               )
               # Removed "Tip: Submit a blank guess" hint
           ),
           div(class = "card",
               h3(class = "card-title", "Your Score"),
               div(class = "score-display", textOutput("score"))
               # Removed current player info display
           )
    ),
    column(8,
           div(class = "card",
               div(class = "game-info",
                   h3(class = "card-title", "Clues"),
                   span(class = "game-info-text", textOutput("clue_count"))
               ),
               div(id = "clues-container", uiOutput("clues"))
           )
    )
  ),
  
  # Footer
  div(class = "footer",
      hr(),
      p(
#        "Created for football fans | ",
        tags$a(href = "https://github.com/DomDF/who_am_i", target = "_blank", 
               tags$i(class = "fa fa-github"), " View on GitHub")
      )
  ),
  
  # Add JavaScript for theme switching and interactivity
  tags$script("
    // Initialize theme from localStorage if available
    $(document).ready(function() {
      const savedTheme = localStorage.getItem('footballGameTheme');
      if (savedTheme === 'dark') {
        document.documentElement.setAttribute('data-theme', 'dark');
        $('#toggle_mode i').removeClass('fa-moon').addClass('fa-sun');
      }
    });
    
    // Handle dark mode toggle
    $(document).on('click', '#toggle_mode', function() {
      const currentTheme = document.documentElement.getAttribute('data-theme');
      const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
      
      document.documentElement.setAttribute('data-theme', newTheme);
      localStorage.setItem('footballGameTheme', newTheme);
      
      // Update icon
      if (newTheme === 'dark') {
        $('#toggle_mode i').removeClass('fa-moon').addClass('fa-sun');
      } else {
        $('#toggle_mode i').removeClass('fa-sun').addClass('fa-moon');
      }
    });
    
    // Confetti animation handler
    Shiny.addCustomMessageHandler('trigger-confetti', function(message) {
      if (message.correct) {
        confetti({
          particleCount: 100,
          spread: 70,
          origin: { y: 0.6 },
          colors: ['#1a5a96', '#4d94d3', '#87ceeb']
        });
      }
    });
    
    // Handle Enter key press - only in the guess input field
    $(document).on('keydown', '#guess', function(e) {
      if(e.which == 13 || e.keyCode == 13) {
        if (!$('#submit_guess').prop('disabled')) {
          e.preventDefault();
          e.stopPropagation();
          $('#submit_guess').click();
        }
        return false;
      }
    });
    
    // Keep focus on input field after submission
    $(document).on('click', '#submit_guess', function() {
      setTimeout(function() {
        if (!$('#guess').prop('disabled') && !$('#guess').is(':focus')) {
          $('#guess').focus();
        }
      }, 200);
    });
  ")
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  current_clue_index <- reactiveVal(1)
  game_over <- reactiveVal(FALSE)
  guess_history <- reactiveVal(list())
  correct_guess <- reactiveVal(FALSE)
  current_game_file <- reactiveVal("clues.txt")
  current_game_data <- reactiveVal(game_data)
  
  # Add debouncing mechanism
  guess_submission_time <- reactiveVal(0)
  
  # Hide the guess history initially (will show after first guess)
  shinyjs::hide("guess-history")
  
  # Display the clue counter
  output$clue_count <- renderText({
    current_data <- current_game_data()
    clues <- current_data$clues
    paste0("Showing ", current_clue_index(), " of ", length(clues), " clues")
  })
  
  # Function to reset the game state
  resetGame <- function() {
    current_clue_index(1)
    game_over(FALSE)
    guess_history(list())
    correct_guess(FALSE)
    shinyjs::hide("guess-history")
    shinyjs::html("result-message", "")
    shinyjs::html("guess-list", "")
    shinyjs::enable("submit_guess")
    shinyjs::enable("guess")
    updateTextInput(session, "guess", value = "")
  }
  
  # Function to load a game based on file path
  loadGame <- function(file_path) {
    tryCatch({
      new_game_data <- loadGameData(file_path)
      current_game_data(new_game_data)
      current_game_file(file_path)
      resetGame()
    }, error = function(e) {
      showNotification(
        "Error loading game file. Using default game instead.",
        type = "error",
        duration = 5
      )
      current_game_data(game_data)
      current_game_file("clues.txt")
      resetGame()
    })
  }
  
  observeEvent(input$play_latest, {
    loadGame("clues.txt")
  })
  
  # Optimized file loading with better error handling
  observeEvent(input$play_random, {
    tryCatch({
      # Try to find archive directory
      archive_paths <- c("archive", "./archive", file.path(dirname(sys.frame(1)$ofile %||% getwd()), "archive"))
      archive_dir <- NULL
      
      for (path in archive_paths) {
        if (dir.exists(path)) {
          archive_dir <- path
          break
        }
      }
      
      if (is.null(archive_dir)) {
        showNotification(
          "Archive directory not found. Using default game.",
          type = "warning",
          duration = 3
        )
        loadGame("clues.txt")
        return()
      }
      
      archive_files <- list.files(path = archive_dir, pattern = "*.txt", full.names = TRUE)
      if (length(archive_files) > 0) {
        random_file <- sample(archive_files, 1)
        loadGame(random_file)
      } else {
        showNotification(
          "No archive files found. Using default game.",
          type = "warning",
          duration = 3
        )
        loadGame("clues.txt")
      }
    }, error = function(e) {
      showNotification(
        "Error accessing archive files. Using default game.",
        type = "error",
        duration = 3
      )
      loadGame("clues.txt")
    })
  })
  
  output$score <- renderText({
    score <- round((1 - (current_clue_index() - 1) / length(current_game_data()$clues)) * 100, 0)
    paste0(score, "%")
  })
  
  # Optimized guess history update with better performance
  updateGuessHistoryUI <- function() {
    guesses <- guess_history()
    if (length(guesses) > 0) {
      shinyjs::show("guess-history")
      # Build HTML string directly instead of using insertUI for better performance
      guess_html <- paste0(
        sapply(guesses, function(g) {
          class_name <- if (g == "[skipped]") "guess-item guess-skip" else "guess-item"
          paste0('<span class="', class_name, '">', htmltools::htmlEscape(g), '</span>')
        }),
        collapse = ""
      )
      shinyjs::html("guess-list", guess_html)
    }
  }
  
  # Handle guess submission with optimized logic
  observeEvent(input$submit_guess, {
    # Debouncing mechanism - prevent rapid submissions
    current_time <- as.numeric(Sys.time()) * 1000
    if (current_time - guess_submission_time() < 500) {
      return()
    }
    guess_submission_time(current_time)
    
    # Early return if game is over
    if (game_over()) return()
    
    # Clear previous messages and disable button
    shinyjs::html("result-message", "")
    shinyjs::disable("submit_guess")
    
    # Process guess
    original_guess_text <- trimws(input$guess)
    processed_guess_for_logic <- tolower(original_guess_text)
    
    # Get current game state
    current_data <- current_game_data()
    targets <- current_data$targets
    clues <- current_data$clues
    target_player <- current_data$target_player
    current_index <- current_clue_index()
    
    # Add to guess history
    history_entry <- if (nchar(original_guess_text) > 0) original_guess_text else "[skipped]"
    current_guesses <- guess_history()
    guess_history(c(current_guesses, history_entry))
    updateGuessHistoryUI()
    
    # Check if guess is correct
    if (nchar(processed_guess_for_logic) > 0) {
      distances <- adist(processed_guess_for_logic, tolower(targets))
      if (min(distances) <= max_acceptable_Levenshtein_distance) {
        # Correct guess - end game
        game_over(TRUE)
        correct_guess(TRUE)
        session$sendCustomMessage(type = "trigger-confetti", message = list(correct = TRUE))
        shinyjs::html("result-message", 
          paste0('<div class="result-message result-success">
            Congratulations! You correctly identified <span class="player-name">', 
            htmltools::htmlEscape(target_player), '</span>!
          </div>'))
        shinyjs::disable("guess")
        updateTextInput(session, "guess", value = "")
        return()
      }
    }
    
    # Handle incorrect or blank guess
    if (current_index < length(clues)) {
      # Reveal next clue
      current_clue_index(current_index + 1)
      
      # Show appropriate message
      if (nchar(processed_guess_for_logic) > 0) {
        shinyjs::html("result-message", 
          '<div class="result-message result-error">Incorrect. A new clue has been revealed.</div>')
      } else {
        shinyjs::html("result-message", 
          '<div class="result-message result-neutral">Skipped. A new clue has been revealed.</div>')
      }
      
      # Re-enable submit button
      shinyjs::enable("submit_guess")
    } else {
      # No more clues - game over
      game_over(TRUE)
      correct_guess(FALSE)
      shinyjs::html("result-message", 
        paste0('<div class="result-message result-error">
          You\'ve used all the clues. The correct answer is: <span class="player-name">', 
          htmltools::htmlEscape(target_player), '</span>
        </div>'))
      shinyjs::disable("guess")
    }
    
    # Clear input for next guess if game continues
    if (!game_over()) {
      updateTextInput(session, "guess", value = "")
    }
  }, ignoreInit = TRUE)
  
  # Optimized clues rendering
  output$clues <- renderUI({
    current_data <- current_game_data()
    clues <- current_data$clues
    current_index <- current_clue_index()
    
    # Pre-build HTML for better performance
    clues_html <- paste0(
      sapply(1:current_index, function(i) {
        paste0(
          '<div class="clue-box" style="animation-delay: ', (i-1)*0.1, 's;">',
          '<span class="clue-number">Clue ', i, ':</span>',
          '<span>', htmltools::htmlEscape(clues[i]), '</span>',
          '</div>'
        )
      }),
      collapse = ""
    )
    
    HTML(clues_html)
  })
  
  observe({
    loadGame("clues.txt")
  })
}

# Run the app
shinyApp(ui = ui, server = server)