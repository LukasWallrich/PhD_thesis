createPipelineDir <- function (NAME) {
  if (dir.exists(here('empirical', '3_pipeline'))){
    pipeline <- here('empirical', '3_pipeline', NAME)
  } else {
    pipeline <- here('3_pipeline', NAME)
  }
  
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
    for (folder in c('out', 'store', 'tmp')){
      dir.create(file.path(pipeline, folder))
    }
  }
  stringr::str_replace(stringr::str_replace(pipeline, here(), ""), "^/", "")
}

getNumOutFolder <- function (num) {
  here(pipelinedir, list.files(here(pipelinedir))[str_detect(list.files(here(pipelinedir)), paste0("^", num))], "out")
}  
