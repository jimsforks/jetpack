#' Show outdated packages
#'
#' @export
#' @examples \dontrun{
#'
#' jetpack::outdated()
#' }
outdated <- function() {
  sandbox({
    prepCommand()

    # update appears to return packages outside project if no packages passed
    packages <- names(getStatus()$lockfile$Package)
    updates <- renv::update(packages=packages, project=renvProject(), check=TRUE)
    showOutdated(updates)
  })
}
