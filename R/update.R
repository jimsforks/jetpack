#' Update a package
#'
#' @param packages Packages to update
#' @param remotes Remotes to update
#' @export
#' @examples \dontrun{
#'
#' jetpack::update("randomForest")
#'
#' jetpack::update(c("randomForest", "DBI"))
#'
#' jetpack::update()
#' }
update <- function(packages=c(), remotes=c()) {
  sandbox({
    prepCommand()

    update_all <- length(packages) == 0

    status <- getStatus()

    if (update_all) {
      packages <- names(status$lockfile$Package)
    }

    versions <- list()
    for (package in packages) {
      package <- getName(package)
      versions[package] <- pkgVersion(status, package)
    }

    if (update_all) {
      dependencies <- names(renv::dependencies()$Package)
      missing <- intersect(dependencies, packages)
      desc <- updateDesc(c(), remotes)
      installHelper(update=dependencies, missing=missing, desc=desc)
    } else {
      desc <- updateDesc(packages, remotes)
      installHelper(update=packages, desc=desc)
    }

    status <- getStatus()
    if (update_all) {
      packages <- names(status$lockfile$Package)
    }

    for (package in packages) {
      package <- getName(package)
      currentVersion <- versions[package]
      newVersion <- pkgVersion(status, package)
      if (is.null(currentVersion)) {
        success(paste0("Installed ", package, " ", newVersion))
      } else {
        success(paste0("Updated ", package, " to ", newVersion, " (was ", currentVersion, ")"))
      }
    }
  })
}
