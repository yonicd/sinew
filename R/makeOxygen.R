#' @title Populate Roxygen2 Skeleton
#' @description Creates roxygen2 skeleton including title, 
#' description, import and other fields for
#' an object in the global environment or a function of an attached namespace.
#' @param obj function or name of function
#' @param add_default boolean to add defaults values to the end of the PARAM 
#' fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen2 fields, 
#' Default: c("details","examples","seealso","rdname","export")
#' @param use_dictionary character, path_to_dictionary, Default: NULL
#' @param print boolean print output to console, Default: TRUE
#' @param \dots arguments to be passed to make_import
#' @details add_fields can include any slot except for the 
#' defaults (title,description,param,return).
#' The order in add_fields determines the order of printout. 
#' The roxygen2 fields to add are list below,
#' for more information go to \href{https://CRAN.R-project.org/package=roxygen2/vignettes/rd.html}{Generating Rd files}.
#' If obj is 'data.frame' or 'tibble' then the fields 
#' c('export','examples','seealso','rdname') will be ignored.
#' 
#' \tabular{ll}{
#' \strong{Field}    \tab \strong{Skeleton}                           \cr
#' author            \tab AUTHOR \[AUTHOR_2\]                           \cr
#' backref           \tab src/filename.cpp                            \cr
#' concept           \tab CONCEPT_TERM_1 \[CONCEPT_TERM_2\]             \cr
#' describeIn        \tab FUNCTION_NAME DESCRIPTION                   \cr
#' details           \tab DETAILS                                     \cr
#' example           \tab path/relative/to/packge/root                \cr
#' export            \tab                                             \cr
#' family            \tab FAMILY_TITLE                                \cr
#' field             \tab FIELD_IN_S4_RefClass DESCRIPTION            \cr
#' format            \tab DATA_STRUCTURE                              \cr
#' importClassesFrom \tab PKG CLASS_a \[CLASS_b\]                       \cr
#' importMethodsFrom \tab PKG METHOD_a \[METHOD_b\]                     \cr
#' include           \tab FILENAME.R \[FILENAME_b.R\]                   \cr
#' inherit           \tab \[PKG::\]SOURCE_FUNCTION \[FIELD_a FIELD_b\]    \cr
#' inheritDotParams  \tab \[PKG::\]SOURCE_FUNCTION                      \cr
#' inheritSection    \tab \[PKG::\]SOURCE_FUNCTION \[SECTION_a SECTION_b\]\cr
#' keywords          \tab KEYWORD_TERM                                \cr
#' name              \tab NAME                                        \cr
#' rdname            \tab FUNCTION_NAME                               \cr
#' references        \tab BIB_CITATION                                \cr
#' section           \tab SECTION_NAME                                \cr
#' source            \tab \\url\{http://somewhere.important.com/\}    \cr
#' slot              \tab SLOTNAME DESCRIPTION                        \cr
#' template          \tab FILENAME                                    \cr
#' templateVar       \tab NAME VALUE                                  \cr
#' useDynLib         \tab PKG \[routine_a routine_b\]
#' }
#' @export
#' @examples
#' makeOxygen(stats::lm)
#' @concept populate
makeOxygen <- function(obj, add_default=TRUE, add_fields=sinew_opts$get("add_fields"), use_dictionary=NULL, print=TRUE, ...) {
  header_add <- sinew_opts$get()

  lbl <- deparse(substitute(obj))
  lbl <- gsub('"', "", lbl)

  if (is.character(obj)) obj <- eval(parse(text = obj,keep.source = TRUE))

  if (inherits(obj, c("data.frame", "tibble"))) {
    cl <- sapply(obj, typeof)

    # Write individual item description templates
    items <- paste0(sprintf("#'   \\item{\\code{%s}}{%s COLUMN_DESCRIPTION}", names(cl), cl), collapse = "\n")

    header <- c(
      title = "#' @title DATASET_TITLE",
      description = "#' @description DATASET_DESCRIPTION",
      format = sprintf("#' @format A data frame with %s rows and %s variables:", nrow(obj), length(cl))
    )

    add_fields <- setdiff(add_fields,c('export','examples','seealso','rdname'))
    
    ret <- sprintf(
      "%s\n%s\n%s%s",
      paste(header, collapse = "\n"),
      sprintf("#' \\describe{\n%s \n#'}", items),
      ifelse(!is.null(add_fields), {
        paste(
          sprintf(
            "#' @%s %s",
            names(header_add[add_fields]),
            header_add[add_fields]
          ),
          collapse = "\n"
        )
      }, ""),
      sprintf('\n"%s"', lbl)
    )
  }

  if (inherits(obj, c("function"))) {
    importList <- list(...)
    importList$script <- obj
    importList$print <- FALSE
    import <- do.call("make_import", importList)
    if (import == "list()") import <- ""

    cutOFF <- switch("cut" %in% names(importList), importList$cut, 3)
    if (import == "") add_fields <- add_fields[!grepl("seealso", add_fields)]
    if ("seealso" %in% add_fields) header_add <- c(header_add, seealso = paste0(make_seealso(obj, cutOFF = cutOFF), collapse = "\n"))

    param_desc <- NULL
    if (!is.null(use_dictionary)) param_desc <- ls_param(obj = obj, dictionary = use_dictionary, print = FALSE)
    fn <- as.list(formals(obj))

    if ("rdname" %in% add_fields) header_add["rdname"] <- lbl

    out <- sapply(names(fn), function(name_y) {
      cl <- class(fn[[name_y]])
      out <- as.character(fn[[name_y]])
      if (cl == "NULL") out <- "NULL"
      if (cl == "character") out <- sprintf("'%s'", as.character(fn[[name_y]]))
      if (cl %in% c("if", "call")) out <- deparse(fn[[name_y]])
      out <- paste0(out, collapse = "\n#'")
      if (add_default) {
        if (nchar(out) > 0) {
          out <- sprintf(", Default: %s", out)
        }

        if (!is.null(use_dictionary) & name_y %in% names(param_desc)) {
          p_desc <- param_desc[name_y]
        } else {
          p_desc <- "PARAM_DESCRIPTION"
        }
        str_out <- sprintf("%s%s", p_desc, out)
      }

      return(str_out)
    })
    params <- sprintf("#' @param %s %s", names(out), out)

    header <- c(
      title = "#' @title FUNCTION_TITLE",
      description = "#' @description FUNCTION_DESCRIPTION"
    )

    footer <- c(return = "#' @return OUTPUT_DESCRIPTION")

    ret <- sprintf(
      "%s\n%s\n%s\n%s\n%s",
      paste(header, collapse = "\n"),
      paste(params, collapse = "\n"),
      footer,
      ifelse(!is.null(add_fields), {
        paste(
          sprintf(
            "#' @%s %s",
            names(header_add[add_fields]),
            header_add[add_fields]
          ),
          collapse = "\n"
        )
      }, ""),
      import
    )
  }

  if (print) writeLines(ret)

  invisible(ret)
}
