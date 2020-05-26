#' Install packages for a project
#'
#' @param deployment Use deployment mode
#' @export
#' @examples \dontrun{
#'
#' jetpack::install()
#' }
install <- function(deployment=FALSE) {
  sandbox({
    prepCommand()

    if (deployment) {
      status <- getStatus()
      packages <- names(status$lockfile$Package)
      dependencies <- renv::dependencies()$Package
      missing <- setdiff(dependencies, packages)
      if (length(missing) > 0) {
        stop(paste("Missing packages:", paste(missing, collapse=", ")))
      }
      suppressWarnings(renv::restore(prompt=FALSE))
      showStatus(status)
    } else {
      installHelper(show_status=TRUE)
    }

    success("Pack complete!")
  })
}
