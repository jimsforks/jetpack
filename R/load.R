#' Load Jetpack
#'
#' @export
#' @keywords internal
load <- function() {
  wd <- getwd()
  dir <- findDir(wd)

  if (is.null(dir)) {
    stopNotPackified()
  }

  # TODO remove these
  library(desc)
  library(docopt)

  venv_dir <- setupEnv(dir)

  # must source from virtualenv directory
  # for RStudio for work properly
  # this should probably be fixed in Packrat
  keepwd({
    setwd(venv_dir)
    quietly(source("renv/activate.R"))
  })

  invisible()
}
