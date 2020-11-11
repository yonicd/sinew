#' @title Remove roxygen2 Comments From an .R File
#' @description Strips .R files of roxygen2 style comments (#')
#' @param .file character, path to an .R file, character vector of length 1
#' @param showonexit logical, show file on exit. Default: TRUE
#' @return Nothing. Overwrites files as a side effect
#' @author Anton Grishin
#' @examples
#' \dontrun{
#' rmOxygen("./myRfunctions/function1.R")
#' }
#' @export
#' @concept utility
#' @import rstudioapi
rmOxygen <- function(.file, showonexit = TRUE) {
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
    if(showonexit)
    file.show(.file)
  }
}
