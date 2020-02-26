#' @title Tabular for roxygen2
#' @description Convert data.frame to roxygen2 tabular format
#' @param df data.frame to convert to table
#' @param header boolean to control if header is created from names(df), Default: TRUE
#' @param \dots arguments to pass to format
#' @return character
#' @source [roxygen2 formatting](https://CRAN.R-project.org/package=roxygen2/vignettes/roxygen2.html)
#' @seealso [format][base::format]
#' @export
#' @examples
#' tabular(mtcars[1:5, 1:5])
#' tabular(mtcars[1:5, 1:5],header=FALSE)
#' @concept utility
tabular <- function(df, header=TRUE, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"

  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  if (header) cols <- lapply(names(cols), function(x) c(sprintf("\\strong{%s}", x), cols[[x]]))
  cols[[1]] <- sprintf("  #' %s", cols[[1]])

  contents <- do.call("paste", c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  structure(paste("#' \\tabular{", paste(col_align, collapse = ""), "}{\n  ", contents, "\n#'}\n", sep = ""),class = c('sinew_tabular','character'))
}

#' @export
print.sinew_tabular <- function(x,...){
  cat(x)
}
