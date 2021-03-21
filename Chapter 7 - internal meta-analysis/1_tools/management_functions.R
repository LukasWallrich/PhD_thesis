  create_pipeline_dir <- function (NAME) {
  pipeline <- here('3_outputs', NAME)
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
}

  stringr::str_replace(stringr::str_replace(pipeline, here(), ''), '^/', '')
}

