# Who am I?

A simple football trivia game, written as a web application in `R` using the `shiny` (https://shiny.posit.co) library. The game is summarised below:

- Guess the current or former professional footballer from the clues.

 - Clues are revealed sequentially, with each incorrect guess.

 - The fewer clues needed to arrive at the correct answer - the higher your score!

Play here: https://allyourbayes.com/posts/who_am_i/

## Adding a puzzle

- Move `who_am_i/clues.txt` to `who_am_i/archive/clues_<n>.txt`, where `<n>` is the next number, then write the new puzzle in `clues.txt`. Puzzle numbers shown in the app come from these file names, and `clues.txt` is always the latest.
- The first line of a clue file lists the accepted answers, separated by commas (the first is the name shown at the end). Each following line is one clue, hardest first.
- `who_am_i/players.txt` holds the names suggested while typing. Answers are added to it automatically.
