#' @title Tabular for roxygen2
#' @description Convert data.frame to roxygen2 tabular format
#' @param df data.frame to convert to table
#' @param header boolean to control if header is created from names(df), Default: TRUE
#' @param ... arguments to pass to format
#' @return character
#' @source \href{https://CRAN.R-project.org/package=roxygen2/vignettes/roxygen2.html}{roxygen2 formatting}
#' @seealso \code{\link[base]{format}}
#' @export
#' @examples
#' cat(tabular(mtcars[1:5, 1:5]))
#' cat(tabular(mtcars[1:5, 1:5],header=FALSE))
tabular <- function(df, header=TRUE, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"

  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  if (header) cols <- lapply(names(cols), function(x) c(sprintf("\\strong{%s}", x), cols[[x]]))
  cols[[1]] <- sprintf("  #' %s", cols[[1]])

  contents <- do.call("paste", c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("#' \\tabular{", paste(col_align, collapse = ""), "}{\n  ", contents, "\n#'}\n", sep = "")
}
