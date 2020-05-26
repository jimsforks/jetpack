#' Migrate from packrat to renv
#'
#' @export
#' @examples \dontrun{
#'
#' jetpack::migrate()
#' }
migrate <- function() {
  sandbox({
    dir <- findDir(getwd())

    dest <- file.path(dir, "renv.lock")
    if (file.exists(dest)) {
      stop("renv.lock already exists.")
    }

    temp_dir <- tempfile()
    dir.create(temp_dir)
    packrat_dir <- file.path(temp_dir, "packrat")
    dir.create(packrat_dir)
    file.copy(file.path(dir, "packrat.lock"), file.path(packrat_dir, "packrat.lock"))
    quietly(renv::migrate(project=temp_dir, packrat=c("lockfile")))
    file.copy(file.path(temp_dir, "renv.lock"), dest)
    cmd <- if (!interactive()) "jetpack install" else "jetpack::install()"
    success(paste0("Lockfile migration successful! To finish migrating:\n1. Delete packrat.lock\n2. Run '", cmd, "'"))
  })
}
