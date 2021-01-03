createPipelineDir <- function (NAME) {
  if (!dir.exists(here('2_pipeline'))) dir.create(here('2_pipeline')) 
  pipeline <- here('2_pipeline', NAME)
  
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
    for (folder in c('out', 'store', 'tmp')){
      dir.create(file.path(pipeline, folder))
    }
  }
  str_replace(str_replace(pipeline, here(), ""), "^/", "")
}

getNumOutFolder <- function (num) {
  here(pipelinedir, list.files(here(pipelinedir))[str_detect(list.files(here(pipelinedir)), paste0("^", num))], "out")
}  
