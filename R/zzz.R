#' @importFrom yaml yaml.load_file
.onLoad <- function(libname, pkgname) {
  config_file <- file.path(getwd(), "_sinewconfig.yml")

  if (file.exists(config_file)) {
    config <- yaml::yaml.load_file(config_file)
    if ("set_fields" %in% names(config)) {
      sinew_opts$set(list(add_fields = config[["set_fields"]]))
      config[["set_fields"]] <- NULL
    }
    if ("add_fields" %in% names(config)) {
      sinew_opts$append(config["add_fields"])
      config[["add_fields"]] <- NULL
    }
    if (length(config) > 0) sinew_opts$set(config)
  }
  
    rc.options(custom.completer = completeme)
    register_completion(thispkg = populate)
}

.onAttach <- function(libname, pkgname) {
  config_file <- file.path(getwd(), "_sinewconfig.yml")

  if (file.exists(config_file)) {
    packageStartupMessage(
      "Setting sinew_opts to _sinewconfig.yml specs\nCurrent add_fields: ",
      paste0(sinew_opts$get("add_fields"), collapse = ",")
    )
  }
}
