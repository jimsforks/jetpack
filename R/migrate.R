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
      message("renv.lock already exists. You should be good to go.")
    } else {
      temp_dir <- tempDir()
      packrat_dir <- file.path(temp_dir, "packrat")
      dir.create(packrat_dir)
      file.copy(file.path(dir, "packrat.lock"), file.path(packrat_dir, "packrat.lock"))
      quietly(renv::migrate(project=temp_dir, packrat=c("lockfile")))
      file.copy(file.path(temp_dir, "renv.lock"), dest)
      cmd <- if (!interactive()) "jetpack install" else "jetpack::install()"
      message(paste0("Lockfile migration successful! To finish migrating:\n1. Delete packrat.lock\n2. Run '", cmd, "'"))
    }
  })
}
