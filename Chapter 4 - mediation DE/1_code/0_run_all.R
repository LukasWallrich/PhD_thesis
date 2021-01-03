if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here("1 - code"), pattern = "\\.R$")[-1]

map(here("1 - code", files), source)

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("1 - code", "last_complete_run.txt"))
