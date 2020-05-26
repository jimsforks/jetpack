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
    outdated <- names(updates$diff)

    if (length(outdated) > 0) {
      for (package in outdated) {
        message(paste0(package, " (latest ", updates$new[[package]]$Version, ", installed ", updates$old[[package]]$Version, ")"))
      }
    } else {
      success("All packages are up-to-date!")
    }
  })
}
