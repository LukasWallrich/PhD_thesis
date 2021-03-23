if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr, callr)

files <- list.files(here("1_code"), pattern = "\\.R$")[-1]

  map(here("1_code", files), function(x) {
    cat(paste("#####################\nNow running: ", x, "\n#####################\n"))
    callr::rscript(x)
})

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("1_code", "last_complete_run.txt"))
