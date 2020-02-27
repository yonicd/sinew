#' @title Create _sinewconfig.yml
#' @description Create _sinewconfig.yml file in project root directory
#' @return nothing
#' @examples
#' \dontrun{
#' create_yml()
#' }
#' @rdname create_yml
#' @export
#' @author Jonathan Sidi
<<<<<<< HEAD
#' @importFrom here here
=======
#' @concept utility
#' @importFrom rstudioapi getActiveProject
>>>>>>> origin/master
create_yml <- function() {
  
  yml <- here::here("_sinewconfig.yml")
  
  ignore <- here::here(".Rbuildignore")

  no_ignore <- !file.exists(ignore)

  if (!file.exists(yml)) {
    file.create(yml)
    message(yml, " created")
  }

  if (no_ignore) {
    file.create(ignore)

    message(ignore, " created")
  }

  current_ignore <- readLines(".Rbuildignore")

  if (!any(grepl("\\^\\.\\*\\\\\\.yml\\$", current_ignore))) {
    cat("^.*\\.yml$", file = ignore, sep = "\n", append = !no_ignore)

    message("^.*\\.yml$ added to ", ignore)
  }
}
