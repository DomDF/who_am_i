# Load the required library
library(shiny); library(shinyjs)

data <- readLines("clues.txt")

targets <- strsplit(x = data[1], split = ", ") |> unlist(); clues <- data[2:length(data)]
target_player <- targets[1]

max_acceptable_Levenshtein_distance <- 3

# Define the UI for the app
ui <- fluidPage(
  
  useShinyjs(), # Load the shinyjs library - Thank you Dean Attali! https://deanattali.com
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Atkinson+Hyperlegible&display=swap"),
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        font-family: 'Atkinson Hyperlegible', sans-serif;
      }
      .panel {
        background-color: #ffffff;
        border-color: #dddddd;
        border-radius: 4px;
        box-shadow: 0 1px 1px rgba(0,0,0,.05);
        padding: 15px;
        margin-bottom: 20px;
      }
      .well {
        background-color: #f5f5f5;
        border: none;
        padding: 15px;
        border-radius: 4px;
      }
    "))
  ),
  titlePanel("Who Am I? The Football Guessing Game", windowTitle = "Who Am I? The Football Guessing Game"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        tags$h4("Enter your guess:"),
        textInput("guess", "", ""),
        actionButton("submit_guess", "Submit Guess"),
        hr(),
        tags$div(id = "result"),
        tags$h4("Current Score:"),
        textOutput("score")
      )
    ),
    mainPanel(
      tags$div(class = "panel",
               tags$h4("Instructions:"),
               verbatimTextOutput("instructions")
      ),
      tags$div(class = "panel",
               tags$h4("Clues:"),
               uiOutput("clues")
      ),
      tags$footer(style = "padding-top: 20px; text-align: center;",
                  tags$a(href = "https://github.com/DomDF/who_am_i", target = "_blank", "GitHub link for this project")
      )
    )
  )
)

# Define the server logic for the app
server <- function(input, output, session) {

  current_clue_index <- reactiveVal(1)
  
  # Display the instructions
  output$instructions <- renderText({
    paste("Guess the footballer (soccer player).",
          "\nA new clue will be revealed after each incorrect guess.",
          "\nThe fewer clues you need, the higher your score.",
          "\nGood luck!")
  })
  
  # Display the current score
  output$score <- renderText({
    score <- round((1 - (current_clue_index() - 1) / length(clues)) * 100, 0)
    paste0(score, "%")
  })
  
  # Update the result text and reveal the next clue when the "Submit Guess" button is clicked
  observeEvent(input$submit_guess, {
    if (adist(input$guess, targets) |> min() <= max_acceptable_Levenshtein_distance) {
      updateTextInput(session, "guess", value = "")
      text_result <- paste0("Congratulations! It's ", target_player, "!")
      shinyjs::disable("submit_guess")
    } else {
      updateTextInput(session, "guess", value = "")
      text_result <- "Incorrect. Please try again."
      
      if (current_clue_index() < length(clues)) {
        current_clue_index(current_clue_index() + 1)
      } else {
        text_result <- paste(text_result, "You've used all the clues. The correct answer is:", target_player)
        shinyjs::disable("submit_guess")
      }
    }
    insertUI(
      selector = "#result",
      where = "beforeEnd",
      ui = tags$div(text_result)
    )
  })
  
  # Display the clues
  output$clues <- renderUI({
    clues_content <- lapply(1:current_clue_index(), function(i) {
      tags$div(paste("Clue", i, ":", clues[i]))
    })
    do.call(tagList, clues_content)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)