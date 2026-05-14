if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

deploy_formals <- names(formals(rsconnect::deployApp))
supports_app_dependencies <- "appDependencies" %in% deploy_formals
deploy_log_level <- "normal"
deploy_app_name <- "DisasterRiskPooling"

# Deploy from an isolated library so renv or older user libraries do not
# override the package versions captured for shinyapps.io.
deploy_lib <- normalizePath(file.path(tempdir(), "rsconnect-library"), winslash = "/", mustWork = FALSE)
if (!dir.exists(deploy_lib)) {
  dir.create(deploy_lib, recursive = TRUE)
}
.libPaths(c(deploy_lib, .Library))
message("Using deployment library: ", deploy_lib)

deploy_pkg_version <- function(pkg) {
  tryCatch(utils::packageVersion(pkg, lib.loc = deploy_lib), error = function(e) NA)
}

# Ensure ggplot2/scales are installed together in the isolated deployment
# library using a standard CRAN install flow.
message("Installing ggplot2/scales into deployment library...")
install.packages(
  c("scales", "ggplot2"),
  repos = "https://cloud.r-project.org",
  lib = deploy_lib,
  dependencies = TRUE
)

resolved_scales <- deploy_pkg_version("scales")
resolved_ggplot2 <- deploy_pkg_version("ggplot2")
message(
  "Resolved deployment versions (deploy_lib): scales=",
  as.character(resolved_scales),
  ", ggplot2=",
  as.character(resolved_ggplot2)
)

if (is.na(resolved_scales) || resolved_scales < "1.4.0") {
  stop("Deployment library has incompatible scales version. Expected >= 1.4.0.")
}

# Parse Imports from DESCRIPTION so rsconnect does not parse renv.lock
# during dependency discovery.
desc <- read.dcf("DESCRIPTION")
imports_raw <- desc[1, "Imports"]
imports <- trimws(unlist(strsplit(imports_raw, ",", fixed = TRUE)))
imports <- imports[nzchar(imports)]
imports <- gsub("[\n\r\t]", "", imports)
imports <- gsub("\\s*\\(.*\\)$", "", imports)

# Also include packages explicitly loaded at startup from global.R.
global_lines <- readLines("global.R", warn = FALSE)
global_libs <- sub("^library\\(([^)]+)\\).*$", "\\1", grep("^library\\([^)]+\\)", global_lines, value = TRUE))

imports <- unique(c(imports, global_libs))

# Ensure dependencies are installed locally; missing packages can become
# null entries in the generated manifest and fail server-side validation.
is_installed <- vapply(imports, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)
missing_imports <- imports[!is_installed]
if (length(missing_imports) > 0) {
  message("Installing missing packages: ", paste(missing_imports, collapse = ", "))
  install.packages(
    missing_imports,
    repos = "https://cloud.r-project.org",
    lib = deploy_lib,
    dependencies = TRUE
  )
}

if (supports_app_dependencies) {
  options(rsconnect.packrat = FALSE)
  message("Deploying with explicit appDependencies:")
  message(paste(imports, collapse = ", "))

  rsconnect::deployApp(
    appDir = ".",
    appName = deploy_app_name,
    appPrimaryDoc = "app.R",
    appDependencies = imports,
    logLevel = deploy_log_level
  )
} else {
  # Older rsconnect versions do not support appDependencies. Force legacy
  # dependency discovery path to avoid renv lockfile parsing failures.
  options(rsconnect.packrat = TRUE)
  message("Older rsconnect detected (no appDependencies argument).")
  message("Falling back to packrat dependency mode for deployment.")

  rsconnect::deployApp(
    appDir = ".",
    appName = deploy_app_name,
    appPrimaryDoc = "app.R",
    logLevel = deploy_log_level
  )
}
