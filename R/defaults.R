new_defaults = function(value = list()) {
  defaults = value
  
  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        setNames(defaults[name], name)
      }
    }
  }
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target
  
  list(get = get, set = set, merge = merge, restore = restore)
}

#' Default and current sinew options
#'
#' Options for functions in the sinew package. When running R code, the object \code{sinew_opts}
#' (default options) is not modified by chunk headers (local chunk options are
#' merged with default options), whereas \code{sinew_opts_current} (current options)
#' changes with different chunk headers and it always reflects the options for
#' the current chunk.
#'
#' Normally we set up the global options once in the first code chunk in a
#' document using \code{sinew_opts$set()}, so that all \emph{latter} chunks will
#' use these options. Note the global options set in one chunk will not affect
#' the options in this chunk itself, and that is why we often need to set global
#' options in a separate chunk.
#'
#' Below is a list of default chunk options, retrieved via
#' \code{sinew_opts$get()}:
#'
#' \Sexpr[results=verbatim]{str(sinew::sinew_opts$get())}
# @references Usage: \url{https://yihui.name/knitr/objects/}
#'
#   A list of available options:
#   \url{https://yihui.name/knitr/options/#chunk_options}
#' @note \code{sinew_opts_current} is read-only in the sense that it does nothing if
#'   you call \code{sinew_opts_current$set()}; you can only query the options via
#'   \code{sinew_opts_current$get()}.
#' @export
#' @examples sinew_opts$get('add_fields')
sinew_opts = new_defaults(list(
  add_fields=c("details", "examples",  "seealso", "rdname", "export")
))

#' @rdname sinew_opts
#' @export
sinew_opts_current = new_defaults()

# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}


