  create_pipeline_dir <- function (NAME) {
  pipeline <- here('3_outputs', NAME)
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
}

  stringr::str_replace(stringr::str_replace(pipeline, here(), ''), '^/', '')
}

  
  take_note <- function(...) {
    x <- list(...) 
    if (length(x)>1) {
      x <- unlist(x)
      cat(paste(x, collapse = ' '))
      notes <<- c(notes, paste(x, collapse = ' '))
    } else {
      x <- x[[1]]
      if ("data.frame" %in% class(x)) {
        res <- capture.output(print.data.frame(data.frame(round_df(x, 3)), row.names = FALSE, right = FALSE))
      } else {
        res <- capture.output(print(x))
      }
      print(res)
      notes <<- c(notes, paste(res, collapse = '\n'))
    }
    invisible(x)
  }
  