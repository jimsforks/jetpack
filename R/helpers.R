checkInsecureRepos <- function() {
  repos <- getOption("repos")
  if (is.list(repos)) {
    repos <- unlist(repos, use.names=FALSE)
  }
  insecure_repos <- repos[startsWith(repos, "http://")]
  for (repo in insecure_repos) {
    warn(paste0("Insecure CRAN repo: ", repo))
  }
}

showOutdated <- function(updates) {
  # returns TRUE instead of empty list no outdated
  if (isTRUE(updates)) {
    outdated <- c()
  } else {
    outdated <- names(updates$diff)
  }

  if (length(outdated) > 0) {
    for (package in outdated) {
      message(paste0(package, " (latest ", updates$new[[package]]$Version, ", installed ", updates$old[[package]]$Version, ")"))
    }
  } else {
    success("All packages are up-to-date!")
  }
}

isTesting <- function() {
  identical(Sys.getenv("TEST_JETPACK"), "true")
}

enablePackrat <- function() {
  # use load (activate updates profile then calls load)
  # no need to call quiet since we already set it globally
  renv::load(renvProject())
}

findDir <- function(path) {
  if (file.exists(file.path(path, "DESCRIPTION"))) {
    path
  } else if (dirname(path) == path) {
    NULL
  } else {
    findDir(dirname(path))
  }
}

getDesc <- function() {
  desc::desc(file=renvProject())
}

getName <- function(package) {
  parts <- strsplit(package, "@")[[1]]
  if (length(parts) != 1) {
    package <- parts[1]
  }
  package
}

getStatus <- function(project=NULL) {
  tryCatch({
    suppressWarnings(renv::status(project=project))
  }, error=function(err) {
    msg <- conditionMessage(err)
    if (grepl("This project has not yet been packified", msg)) {
      stopNotPackified()
    } else {
      stop(msg)
    }
  })
}

installHelper <- function(desc=NULL, show_status=FALSE, update=NULL, missing=NULL) {
  if (is.null(desc)) {
    desc <- getDesc()
  }

  # use a temporary directly
  # this way, we don't update DESCRIPTION
  # until we know it was successful
  dir <- renvProject()
  temp_desc <- file.path(dir, "DESCRIPTION")
  desc$write(temp_desc)
  # strip trailing whitespace
  lines <- trimws(readLines(temp_desc), "r")
  writeLines(lines, temp_desc)

  if (is.null(update)) {
    renv::install(project=renvProject())
  } else {
    if (!is.null(missing)) {
      renv::install(packages=missing, project=renvProject())
    }
    renv::update(packages=update, project=renvProject())
  }

  renv::snapshot(project=renvProject(), prompt=FALSE)

  # copy back after successful
  jetpack_dir <- getOption("jetpack_dir")
  file.copy(file.path(renvProject(), "DESCRIPTION"), file.path(jetpack_dir, "DESCRIPTION"), overwrite=TRUE)
  file.copy(file.path(renvProject(), "renv.lock"), file.path(jetpack_dir, "renv.lock"), overwrite=TRUE)

  if (show_status) {
    status <- renv::status(project=renvProject())
    showStatus(status)
  }
}

isWindows <- function() {
  .Platform$OS.type != "unix"
}

oneLine <- function(x) {
  gsub("\n", " ", x)
}

noPackrat <- function() {
  if (packratOn()) {
    renv::deactivate(renvProject())
  }
}

renvProject <- function() {
  getOption("jetpack_venv")
}

packified <- function() {
  file.exists(file.path(renvProject(), "renv"))
}

pkgVersion <- function(status, name) {
  row <- status$lockfile$Package[[name]]
  if (is.null(row)) {
    stop(paste0("Cannot find package '", name, "' in DESCRIPTION file"))
  }
  row$Version
}

pkgRemove <- function(name) {
  if (name %in% rownames(utils::installed.packages())) {
    suppressMessages(utils::remove.packages(name))
  }
}

packratOn <- function() {
  !is.na(Sys.getenv("RENV_PROJECT", unset=NA))
}

prepCommand <- function() {
  dir <- findDir(getwd())

  if (is.null(dir)) {
    stopNotPackified()
  }

  options(jetpack_dir=dir)
  venv_dir <- setupEnv(dir)

  # copy files
  file.copy(file.path(dir, "DESCRIPTION"), file.path(venv_dir, "DESCRIPTION"), overwrite=TRUE)
  file.copy(file.path(dir, "renv.lock"), file.path(venv_dir, "renv.lock"), overwrite=TRUE)

  if (!packratOn()) {
    if (interactive()) {
      stop("renv must be loaded to run this. Restart your R session to continue.")
    } else {
      enablePackrat()
    }
  }

  ensureRepos()
  checkInsecureRepos()
}

ensureRepos <- function() {
  repos <- getOption("repos", list())
  if (!is.na(repos["CRAN"]) && repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org/"
    options(repos=repos)
  }
}

sandbox <- function(code) {
  invisible(eval(code))
}

silenceWarnings <- function(msgs, code) {
  unsolved_error <- FALSE
  muffle <- function(w) {
    if (any(sapply(msgs, function(x) { grepl(x, conditionMessage(w), fixed=TRUE) }))) {
      unsolved_error <<- TRUE
      invokeRestart("muffleWarning")
    }
  }
  res <- withCallingHandlers(code, warning=muffle)

  if (unsolved_error) {
    warn("Command successful despite error above (unsolved Jetpack issue)")
  }

  res
}

showStatus <- function(status) {
  for (row in status$library$Packages) {
    message(paste0("Using ", row$Package, " ", row$Version))
  }
}

stopNotPackified <- function() {
  cmd <- if (!interactive()) "jetpack init" else "jetpack::init()"
  stop(paste0("This project has not yet been packified.\nRun '", cmd, "' to init."))
}

stopNotMigrated <- function() {
  cmd <- if (!interactive()) "jetpack migrate" else "jetpack::migrate()"
  stop(paste0("This project has not yet been migrated to renv.\nRun '", cmd, "' to migrate."))
}

success <- function(msg) {
  cat(color(paste0(msg, "\n"), "green"))
}

color <- function(message, color) {
  if (interactive() || isatty(stdout())) {
    color_codes = list(red=31, green=32, yellow=33)
    paste0("\033[", color_codes[color], "m", message, "\033[0m")
  } else {
    message
  }
}

tempDir <- function() {
  dir <- file.path(tempdir(), sub("\\.", "", paste0("jetpack", as.numeric(Sys.time()))))
  dir.create(dir)
  dir
}

updateDesc <- function(packages, remotes, desc=NULL) {
  if (is.null(desc)) {
    desc <- getDesc()
  }

  for (remote in remotes) {
    desc$add_remotes(remote)
  }

  for (package in packages) {
    parts <- strsplit(package, "@")[[1]]
    version <- NULL
    version_str <- "*"
    if (length(parts) != 1) {
      package <- parts[1]
      version <- parts[2]
      version_str <- paste("==", version)
    }

    desc$set_dep(package, "Imports", version=version_str)
  }

  desc
}

warn <- function(msg) {
  cat(color(paste0(msg, "\n"), "yellow"))
}

venvDir <- function(dir) {
  # similar logic as Pipenv
  if (isTesting()) {
    venv_dir <- file.path(tempdir(), "renvs")
  } else if (isWindows()) {
    venv_dir <- "~/.renvs"
  } else {
    venv_dir <- file.path(Sys.getenv("XDG_DATA_HOME", "~/.local/share"), "renvs")
  }

  # TODO better algorithm, but keep dependency free
  dir_hash <- sum(utf8ToInt(dir)) + 1
  venv_name <- paste0(basename(dir), "-", dir_hash)
  file.path(venv_dir, venv_name)
}

quietly <- function(code) {
  utils::capture.output(suppressMessages(code))
}

keepwd <- function(code) {
  wd <- getwd()
  tryCatch(code, finally={ setwd(wd) })
}

loadExternal <- function(package) {
  lib_paths <- getOption("jetpack_lib")
  loadNamespace(package, lib.loc=lib_paths)
}

setupEnv <- function(dir=getwd(), init=FALSE) {
  ensureRepos()

  venv_dir <- venvDir(dir)
  if (init && file.exists(venv_dir) && !file.exists(file.path(dir, "renv.lock"))) {
    # remove previous virtual env
    unlink(venv_dir, recursive=TRUE)
  }
  if (!file.exists(venv_dir)) {
    dir.create(venv_dir, recursive=TRUE)
  }

  options(jetpack_lib=.libPaths())

  # quiet output
  options(renv.verbose=FALSE, renv.config.synchronized.check = FALSE, jetpack_venv=venv_dir)

  # initialize packrat
  if (!packified()) {
    if (file.exists(file.path(dir, "packrat.lock")) && !file.exists(file.path(dir, "renv.lock"))) {
      stopNotMigrated()
    }

    message("Creating virtual environment...")

    file.copy(file.path(dir, "DESCRIPTION"), file.path(venv_dir, "DESCRIPTION"), overwrite=TRUE)

    # restore wd after init changes it
    # TODO find way to suppress output from init
    keepwd(quietly(renv::init(project=venv_dir, bare=TRUE, restart=FALSE, settings=list(snapshot.type = "explicit"))))
    quietly(renv::snapshot(prompt=FALSE, force=TRUE))

    # reload desc
    # TODO remove this dependency
    if (interactive()) {
      loadExternal("desc")
    }
  }

  # if (!file.exists(file.path(dir, "renv.lock"))) {
  #   file.copy(file.path(renvProject(), "renv.lock"), file.path(dir, "renv.lock"))
  # }

  venv_dir
}
