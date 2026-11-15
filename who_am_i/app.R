library(shiny)

# A guess is accepted if it is within this many edits of any accepted answer
max_acceptable_Levenshtein_distance <- 2

# Link included in shared results
share_url <- "https://allyourbayes.com/posts/who_am_i/"

# ---- Puzzle data ------------------------------------------------------------

# Each clue file: first line is a comma-separated list of accepted answers
# (the first is the display name), followed by one clue per line.
read_puzzle <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2) stop("Invalid clue file: ", file_path)

  targets <- trimws(strsplit(lines[1], ",", fixed = TRUE)[[1]])
  targets <- targets[nzchar(targets)]
  if (length(targets) == 0) stop("No answers found in: ", file_path)

  list(targets = targets, clues = lines[-1], answer = targets[1])
}

fallback_puzzle <- list(
  targets = c("Lionel Messi", "Messi"),
  clues = c(
    "I am from Argentina",
    "I have won multiple Ballon d'Or awards",
    "I played for Barcelona for most of my career",
    "I won the 2022 FIFA World Cup"
  ),
  answer = "Lionel Messi"
)

# Archive puzzles keep the number in their file name; clues.txt is the latest
# puzzle and takes the next number.
load_puzzles <- function() {
  puzzles <- list()
  archive_files <- list.files("archive", pattern = "^clues_[0-9]+\\.txt$", full.names = TRUE)
  for (file_path in archive_files) {
    id <- as.integer(gsub("[^0-9]", "", basename(file_path)))
    puzzle <- tryCatch(read_puzzle(file_path), error = function(e) {
      warning(conditionMessage(e))
      NULL
    })
    if (!is.null(puzzle)) puzzles[[as.character(id)]] <- puzzle
  }

  latest_id <- max(c(0L, as.integer(names(puzzles)))) + 1L
  puzzles[[as.character(latest_id)]] <- tryCatch(read_puzzle("clues.txt"), error = function(e) {
    warning(conditionMessage(e))
    fallback_puzzle
  })

  puzzles
}

puzzles <- load_puzzles()
puzzle_ids <- sort(as.integer(names(puzzles)))
latest_id <- max(puzzle_ids)

# Names offered as suggestions while typing. Every answer is added to the
# general list in players.txt so that no puzzle is missing from it.
suggestion_name <- function(targets) {
  n_words <- lengths(strsplit(targets, " +"))
  usual <- targets[n_words %in% 2:3]
  if (length(usual) > 0) return(usual[1])
  single <- targets[n_words == 1]
  if (length(single) > 0) return(single[which.min(nchar(single))])
  targets[1]
}

load_players <- function() {
  listed <- if (file.exists("players.txt")) {
    trimws(readLines("players.txt", warn = FALSE, encoding = "UTF-8"))
  } else {
    character(0)
  }
  answers <- vapply(puzzles, function(p) suggestion_name(p$targets), character(1))
  players <- c(listed[nzchar(listed)], unname(answers))
  sort(players[!duplicated(tolower(players))])
}

client_config <- jsonlite::toJSON(
  list(
    players = as.list(load_players()),
    ids = as.list(puzzle_ids),
    latest = latest_id,
    url = share_url
  ),
  auto_unbox = TRUE
)

# ---- UI ---------------------------------------------------------------------

icon_button <- function(id, icon_name, label) {
  tags$button(id = id, type = "button", class = "icon-btn", `aria-label` = label, title = label,
              icon(icon_name))
}

sheet <- function(id, title, ...) {
  tags$dialog(
    id = id, class = "sheet", `aria-labelledby` = paste0(id, "-title"),
    div(class = "sheet-body",
        div(class = "sheet-head",
            h2(id = paste0(id, "-title"), class = "sheet-title", title),
            tags$button(type = "button", class = "icon-btn", `data-close` = NA, `aria-label` = "Close",
                        icon("xmark"))
        ),
        ...
    )
  )
}

stat <- function(id, label) {
  div(div(id = id, class = "stat-num", "0"), div(class = "stat-label", label))
}

page_description <- "Guess the football legend in as few clues as you can."

ui <- fluidPage(
  title = "Who Am I? | Football guessing game",
  theme = bslib::bs_theme(version = 5),

  tags$head(
    # Apply the saved or system theme before first paint
    tags$script(HTML("
      (function () {
        var theme = null;
        try { theme = localStorage.getItem('footballGameTheme'); } catch (e) {}
        if (theme !== 'dark' && theme !== 'light') {
          theme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
        }
        document.documentElement.setAttribute('data-theme', theme);
      })();
    ")),
    tags$meta(name = "description", content = page_description),
    tags$meta(property = "og:title", content = "Who Am I?"),
    tags$meta(property = "og:description", content = page_description),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(name = "theme-color", content = "#0b7a4b"),
    tags$link(rel = "icon", href = paste0(
      "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E",
      "%3Ctext y='.9em' font-size='90'%3E%E2%9A%BD%3C/text%3E%3C/svg%3E"
    )),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=Archivo:wght@800;900",
      "&family=Atkinson+Hyperlegible:wght@400;700&display=swap"
    )),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js",
                defer = NA)
  ),

  tags$main(
    class = "app",

    tags$header(
      class = "topbar",
      div(class = "brand",
          div(class = "brand-mark", icon("futbol")),
          div(class = "brand-text",
            h1(class = "brand-name", "Who Am I?"),
            div(id = "puzzle-label", class = "brand-sub", HTML("&nbsp;"))
          )
      ),
      div(class = "topbar-actions",
          tags$button(id = "streak-pill", type = "button", class = "streak-pill", hidden = NA,
                      icon("fire"), span(id = "streak-count", "0")),
          icon_button("stats-btn", "chart-simple", "Your stats"),
          icon_button("archive-btn", "table-cells", "All puzzles"),
          icon_button("help-btn", "question", "How to play"),
          tags$button(id = "theme-btn", type = "button", class = "icon-btn",
                      `aria-label` = "Switch colour theme", title = "Switch colour theme",
                      span(class = "theme-moon", icon("moon")),
                      span(class = "theme-sun", icon("sun")))
      )
    ),

    p(class = "tagline", page_description),

    div(class = "scorebar",
        div(class = "score-progress",
          div(id = "progress-label", class = "progress-label", HTML("&nbsp;")),
          div(id = "pips", class = "pips")
        ),
        div(class = "points",
            span(id = "points-value", class = "points-value"),
            span(id = "points-label", class = "points-label", "points on offer")
        )
    ),

    tags$ol(id = "clues", class = "clues", `aria-live` = "polite"),

    div(id = "dock", class = "dock",
        div(id = "feedback", class = "feedback", role = "status"),
        div(id = "chips", class = "chips", `aria-label` = "Previous guesses"),
        tags$form(id = "guess-form", class = "guess-form", autocomplete = "off",
                  tags$ul(id = "suggestions", class = "suggestions", role = "listbox", hidden = NA),
                  tags$input(id = "guess-input", class = "guess-input", type = "text",
                             placeholder = "Name the player", maxlength = "80",
                             autocomplete = "off", autocapitalize = "words", spellcheck = "false",
                             enterkeyhint = "go", role = "combobox",
                             `aria-label` = "Your guess", `aria-controls` = "suggestions",
                             `aria-expanded` = "false", disabled = NA),
                  tags$button(id = "guess-btn", type = "submit", class = "btn-x btn-primary-x",
                              disabled = NA, "Guess")
        ),
        tags$button(id = "skip-btn", type = "button", class = "skip-btn", disabled = NA,
                    icon("forward"), " Skip and reveal the next clue")
    ),

    tags$section(id = "end-card", class = "end-card", hidden = NA, `aria-live` = "polite",
                 div(id = "end-eyebrow", class = "end-eyebrow"),
                 div(id = "end-name", class = "end-name"),
                 div(id = "end-meta", class = "end-meta"),
                 div(id = "end-grid", class = "end-grid", `aria-hidden` = "true"),
                 div(class = "end-actions",
                     tags$button(id = "share-btn", type = "button", class = "btn-x btn-primary-x",
                                 icon("share-nodes"), span(id = "share-label", "Share")),
                     tags$button(id = "next-btn", type = "button", class = "btn-x btn-ghost-x",
                                 icon("forward"), span(id = "next-label", "Next puzzle"))
                 ),
                 div(id = "end-note", class = "end-note")
    ),

    tags$footer(class = "footer",
                tags$a(href = "https://github.com/DomDF/who_am_i", target = "_blank", rel = "noopener",
                       icon("github"), " View on GitHub"))
  ),

  sheet("stats-dialog", "Your stats",
        div(class = "stat-row",
            stat("stat-played", "Played"),
            stat("stat-win", "Solved"),
            stat("stat-streak", "Streak"),
            stat("stat-best", "Best streak")
        ),
        div(class = "sheet-sub", "Clues needed"),
        div(id = "dist")
  ),

  sheet("archive-dialog", "All puzzles",
        div(id = "archive-grid", class = "archive-grid"),
        div(id = "archive-legend", class = "legend")
  ),

  sheet("help-dialog", "How to play",
        tags$ul(class = "help-list",
                tags$li("Each puzzle is a current or former professional footballer."),
                tags$li("You start with one clue. Every wrong guess or skip unlocks the next one."),
                tags$li("The fewer clues you need, the more points you score."),
                tags$li("Surnames and small spelling mistakes are accepted."),
                tags$li("Solve puzzles in a row to build a streak. Your stats are saved in this browser.")
        )
  ),

  tags$script(HTML(paste0("window.WAI = ", client_config, ";"))),
  tags$script(src = "game.js")
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  # Per-session game state. The answer and unrevealed clues never leave the
  # server until the game is over.
  state <- reactiveValues(id = NULL, index = 1L, over = FALSE)

  observeEvent(input$load_puzzle, {
    id <- suppressWarnings(as.integer(input$load_puzzle$id))
    if (length(id) != 1 || is.na(id) || !(id %in% puzzle_ids)) id <- latest_id
    puzzle <- puzzles[[as.character(id)]]

    state$id <- id
    state$index <- 1L
    state$over <- FALSE

    session$sendCustomMessage("new-game", list(
      id = id,
      total = length(puzzle$clues),
      clue = puzzle$clues[1]
    ))
  })

  observeEvent(input$guess, {
    if (is.null(state$id) || state$over) return()

    puzzle <- puzzles[[as.character(state$id)]]
    total <- length(puzzle$clues)
    guess <- input$guess$text
    guess <- if (is.character(guess) && length(guess) == 1) substr(trimws(guess), 1, 80) else ""

    is_correct <- nzchar(guess) &&
      min(adist(tolower(guess), tolower(puzzle$targets))) <= max_acceptable_Levenshtein_distance

    result <- list(id = state$id, guess = guess)

    if (is_correct || state$index >= total) {
      # Game over: reveal the answer and any clues that were still locked
      state$over <- TRUE
      result$status <- if (is_correct) "correct" else if (nzchar(guess)) "wrong" else "skipped"
      result$over <- TRUE
      result$won <- is_correct
      result$answer <- puzzle$answer
      result$rest <- as.list(puzzle$clues[seq_len(total - state$index) + state$index])
    } else {
      state$index <- state$index + 1L
      result$status <- if (nzchar(guess)) "wrong" else "skipped"
      result$over <- FALSE
      result$index <- state$index
      result$clue <- puzzle$clues[state$index]
    }

    session$sendCustomMessage("guess-result", result)
  })
}

shinyApp(ui = ui, server = server)
