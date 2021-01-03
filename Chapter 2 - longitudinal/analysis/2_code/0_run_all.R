if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here("empirical", "2_code"), pattern = "\\.R$")[-1]

map(here("empirical", "2_code", files), source)

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("empirical", "2_code", "last_complete_run.txt"))
