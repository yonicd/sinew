# forked from https://github.com/yihui/knitr/blob/master/R/defaults.R
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
  resolve = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    dots
  }
  set = function(...) {
    dots = resolve(...)
    if (length(dots)) defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target
  append = function(...) {
    dots = resolve(...)
    for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
    if (length(dots)) defaults <<- merge(dots)
    invisible(NULL)
  }
  
  list(get = get, set = set, append = append, merge = merge, restore = restore)
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
#' A list of default chunk options, can be retrieved via
#' \code{sinew_opts$get()}
#' 
#' @note \code{sinew_opts_current} is read-only in the sense that it does nothing if
#'   you call \code{sinew_opts_current$set()}; you can only query the options via
#'   \code{sinew_opts_current$get()}.
#' @export
#' @concept opts
#' @examples sinew_opts$get()
sinew_opts <- new_defaults(list(
  add_fields = c("details", "examples", "seealso", "rdname", "export"),
  cut = NULL,
  pretty_print = TRUE,
  markdown_links = FALSE,
  author = "AUTHOR [AUTHOR_2]",
  backref = "src/filename.cpp",
  concept = "CONCEPT_TERM_1 [CONCEPT_TERM_2]",
  describeIn = "FUNCTION_NAME DESCRIPTION",
  details = "DETAILS",
  # evalRd           ="",
  example = "path_to_file/relative/to/packge/root",
  examples = "\n#' \\dontrun{\n#' if(interactive()){\n#'  #EXAMPLE1\n#'  }\n#' }",
  export = "",
  # exportClass      ="",
  # exportMethod     ="",
  family = "FAMILY_TITLE",
  field = "FIELD_IN_S4_RefClass DESCRIPTION",
  format = "DATA_STRUCTURE",
  importClassesFrom = "PKG CLASS_a [CLASS_b]",
  importMethodsFrom = "PKG METHOD_a [METHOD_b]",
  include = "FILENAME.R [FILENAME_b.R]",
  inherit = "[PKG::]SOURCE_FUNCTION [FIELD_a FIELD_b]",
  inheritDotParams = "[PKG::]SOURCE_FUNCTION",
  inheritSection = "[PKG::]SOURCE_FUNCTION [SECTION_a SECTION_b]",
  keywords = "KEYWORD_TERM",
  name = "NAME",
  # note             ="",
  # noRd             ="",
  # rawRd            ="",
  # rawNamespace     ="",
  rdname = "FUNCTION_NAME",
  references = "BIB_CITATION",
  section = "SECTION_NAME",
  source = "\\url{http://somewhere.important.com/}",
  slot = "SLOTNAME DESCRIPTION",
  template = "FILENAME",
  templateVar = "NAME VALUE",
  useDynLib = "PKG [ROUTINE_a ROUTINE_b]"
))

#' @rdname sinew_opts
#' @export
sinew_opts_current <- new_defaults()

# merge elements of y into x with the same names
merge_list <- function(x, y) {
  x[names(y)] <- y
  x
}

setNames <- function(object = nm, nm) {
  names(object) <- nm
  object
}
