#' @title set a default author for makeOxygen
#' @description function allowing to set a default value for the @author field created by
#'  makeOxygen (or#' reset it to default).
#' @param author `character` or object of class `person` to be used to automatically fill
#' the @author field when calling `makeOxygen`. If NULL, resets the @author field to
#' ("AUTHOR [AUTHOR_2]"), Default: NULL
#' @examples
#' \dontrun{
#'  # Create an "author" variable including two authors
#'  author <- c(person("John", "Smith, phD (2017)", role = "aut"),
#'              person(c("Mary", "D."), "Ripley", role = c("ctb")))
#'
#'  # set it as default for makeOxygen
#'  setDefaultAuthor(author)
#'
#'  # include @author as a default field
#'  setDefaultAddfields(c("details", "export", "author"))
#'
#'  # create Rd skeleton, including automatically filled-in author
#'  makeOxygen(stats::lm)
#'  }
#' @rdname setDefaultAuthor
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

setDefaultAuthor <- function(author = NULL) {
  if (is.null(author)) {
    # If `author` is not passed, set/reset author to default.
    options(sinew_DefaultAuthor = "AUTHOR [AUTHOR_2]")
  } else {
    # if `author` is of class "person", collapse it to a character representation,
    # allowing "nice" formatting of multiple authors
    if (inherits(author, "person")) {
      author <- as.character(paste(author, collapse = ";\\cr \n#' "))
    }
    # check if `author` is character. If not, abort.
    # Otherwise, set options()$sinew_DefaultAuthor
    if (!is.character(author)) {
      stop("sinew --> `add_author` must be a character string. Aborting.")
    } else {
      message("sinew --> Setting default `@author` to: ", author)
      options(sinew_DefaultAuthor = author)
    }
  }
}
