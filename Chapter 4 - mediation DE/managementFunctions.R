createPipelineDir <- function (NAME) {
  if (!dir.exists(here('2_pipeline'))) dir.create(here('2_pipeline')) 
  pipeline <- here('2_pipeline', NAME)
  
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
  }
  str_replace(str_replace(pipeline, here(), ""), "^/", "")
}

getNumOutFolder <- function (num) {
  here(pipelinedir, list.files(here(pipelinedir))[str_detect(list.files(here(pipelinedir)), paste0("^", num))], "out")
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


trace_random <- function() {
  invisible(addTaskCallback(local({
    last <- .GlobalEnv$.Random.seed
    
    function(...) {
      curr <- .GlobalEnv$.Random.seed
      if (!identical(curr, last)) {
        msg <- "NOTE: .Random.seed changed"
        if (requireNamespace("crayon", quietly=TRUE)) msg <- crayon::blurred(msg)
        message(msg)
        last <<- curr
      }
      TRUE
    }
  }), name = "RNG tracker"))
}
