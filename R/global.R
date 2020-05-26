prepGlobal <- function() {
  noPackrat()
  ensureRepos()
  checkInsecureRepos()
  options(renv.verbose=FALSE)
}

globalAdd <- function(packages, remotes) {
  globalInstallHelper(packages, remotes)

  for (package in packages) {
    package <- getName(package)
    success(paste0("Installed ", package, " ", utils::packageVersion(package)))
  }
}

globalInstallHelper <- function(packages, remotes=c()) {
  # create temporary directory, write description, install deps
  dir <- tempDir()
  desc <- desc::desc("!new")
  updateDesc(packages, remotes, desc=desc)
  desc$write(file.path(dir, "DESCRIPTION"))

  # TODO don't remove for add command
  for (package in packages) {
    if (package %in% rownames(utils::installed.packages())) {
      suppressMessages(utils::remove.packages(package))
    }
  }

  renv::install(packages=packages, project=dir)
}

globalList <- function() {
  packages <- as.data.frame(utils::installed.packages())
  packages <- packages[order(tolower(packages$Package)), ]
  for (i in 1:nrow(packages)) {
    row <- packages[i, ]
    message(paste0("Using ", row$Package, " ", row$Version))
  }
}

globalOutdatedPackages <- function() {
  packages <- as.data.frame(utils::installed.packages())
  packages <- packages[packages$Priority != "base" | is.na(packages$Priority),]
  packages <- rownames(packages)
  renv::update(packages=packages, check=TRUE, project=tempDir())
}

globalOutdated <- function() {
  showOutdated(globalOutdatedPackages())
}

globalRemove <- function(packages) {
  for (package in packages) {
    suppressMessages(utils::remove.packages(package))
  }
  for (package in packages) {
    success(paste0("Removed ", package, "!"))
  }
}

globalUpdate <- function(packages, remotes, verbose) {
  if (length(packages) == 0) {
    outdated <- globalOutdatedPackages()

    if (nrow(outdated) > 0) {
      for (i in 1:nrow(outdated)) {
        row <- outdated[i, ]
        package <- row$package
        utils::install.packages(package, quiet=!verbose)
        newVersion <- as.character(utils::packageVersion(package))
        success(paste0("Updated ", package, " to ", newVersion, " (was ", row$installed, ")"))
      }
    } else {
      success("All packages are up-to-date!")
    }
  } else {
    versions <- list()
    for (package in packages) {
      package <- getName(package)
      versions[package] <- as.character(utils::packageVersion(package))
    }

    globalInstallHelper(packages, remotes)

    for (package in packages) {
      package <- getName(package)
      currentVersion <- versions[package]
      newVersion <- as.character(utils::packageVersion(package))
      success(paste0("Updated ", package, " to ", newVersion, " (was ", currentVersion, ")"))
    }
  }
}
