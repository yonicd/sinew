#' @title Remove roxygen2 Comments From an .R File
#' @description Strips .R files of roxygen2 style comments (#')
#' @param .file, path to an .R file, character vector of length 1
#' @return Nothing. Overwrites files as a side effect
#' @author Anton Grishin
#' @examples
#' \dontrun{
#' rmOxygen("./myRfunctions/function1.R")
#' }
#' @export
#' @import rstudioapi
rmOxygen <- function(.file) {
  if (!file.exists(.file)) stop("Enter a valid path to .R file!", call. = FALSE)
  if (!endsWith(.file, ".R")) {
    stop(paste0(
      dQuote(basename(.file)),
      " is not an .R file!"
    ), call. = FALSE)
  }
  lines <- readLines(.file)
  lines <- lines[!grepl("^\\s*#'", lines)]
  writeLines(lines, .file)
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(.file)
  } else {
    file.show(.file)
  }
}
