if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here("2_code"), pattern = "\\.R$")[-1]

# Allow clearing globale
choice <- readline(prompt = "Should global environment be cleared between each script? (Y/N): ")

if (choice %in% c("Y", "y")) {
  
map(here("2_code", files), function(x) {
  rm(list=setdiff(ls(envir = globalenv()), "x"), envir = globalenv())
  source(x)
  })
} else {
  map(here("2_code", files), source)
}

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("2_code", "last_complete_run.txt"))
