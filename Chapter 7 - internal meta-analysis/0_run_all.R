if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here(), pattern = "\.R$")[-1]

map(here(files), source)

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("last_complete_run.txt"))

