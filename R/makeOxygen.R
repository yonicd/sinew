obj_lbl <- function(obj, env = parent.frame()) {
  gsub('"', "", deparse(substitute(obj, env = env)))
}

#' @title Populate Roxygen2 Skeleton
#' @description Creates roxygen2 skeleton including title, description, import
#'   and other fields for an object in the global environment or a function of
#'   an attached namespace.
#' @param obj function or name of function, a data frame, or a vector (strings
#'   are interpreted as function names).
#' @param title,description Optional title and description values to use
#' @param add_default boolean to add defaults values to the end of the PARAM 
#' fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen2 fields, 
#' Default: c("details","examples","seealso","rdname","export")
#' @param use_dictionary character, path_to_dictionary, Default: NULL
#' @param use_labels boolean to use label attribute of data frame columns to
#'   fill column description values. Ignored if obj is a function or vector.
#'   Default: FALSE.
#' @param markdown boolean to return roxygen2 skeleton with Markdown formatting,
#'   Default: FALSE
#' @param print boolean print output to console, Default: TRUE
#' @param copy boolean copy output to clipboard, Default: [interactive()]
#' @inheritParams roxygen2md::markdownify
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
#' @importFrom cli cli_code cli_alert_success
makeOxygen <- function(
    obj,
    title = NULL,
    description = NULL,
    add_default=TRUE,
    add_fields=sinew_opts$get("add_fields"),
    use_dictionary=NULL,
    use_labels=FALSE,
    markdown=FALSE,
    scope = c("full", "simple", "unlink", "indent", "none"),
    print=TRUE,
    copy=interactive(),
    ...
) {
  
  if (is.character(obj) && length(obj) == 1) {
    obj <- eval(parse(text = obj,keep.source = TRUE))
  }

  if (is.vector(obj)) {
    ret <- prep_vec_roxy(
      obj,
      title = title,
      description = description,
      add_fields = add_fields,
      ...
    )
  }
  
  if (inherits(obj, c("data.frame", "tibble"))) {
    ret <- prep_tbl_roxy(
      obj,
      title = title,
      description = description,
      add_fields = add_fields,
      ...
    )
  }

  if (inherits(obj, c("function"))) {
    ret <- prep_fn_roxy(
      obj,
      title = title,
      description = description,
      add_default = add_default,
      add_fields = add_fields,
      use_dictionary = use_dictionary,
      ...
    )
  }
  
  if (markdown && requireNamespace("roxygen2md", quietly = TRUE)) {
    ret <- roxygen2md::markdownify(ret, scope = scope)
  }

  if (print) cli_code(ret)
  
  if (copy && requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
   suppressPackageStartupMessages(clipr::write_clip(ret))
    cli_alert_success("Copied to clipboard")
  }

  invisible(ret)
}

#' @rdname makeOxygen
#' @name make_oxygen
#' @export
make_oxygen <- makeOxygen

#' @noRd
prep_fn_roxy <- function(obj,
                         title = NULL,
                         description = NULL,
                         add_default = TRUE,
                         add_fields=sinew_opts$get("add_fields"),
                         use_dictionary=NULL,
                         header_add = sinew_opts$get(),
                         env = parent.frame(),
                         ...) {
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
      out <- sprintf("%s%s", p_desc, out)
    }
    
    return(out)
  })
  
  params <- sprintf("#' @param %s %s", names(out), out)
  
  header <- set_roxy_header(
    title = title,
    description = description,
    defaults = c("FUNCTION_TITLE", "FUNCTION_DESCRIPTION"),
    collapse = "\n"
  )
  
  footer <- c(return = "#' @returns OUTPUT_DESCRIPTION")
  
  if ("rdname" %in% add_fields) header_add["rdname"] <- obj_lbl(obj, env = env)
  
  add_fields <- set_roxy_add_fields(
    add_fields = add_fields,
    header_add = header_add
  )
  
  sprintf(
    "%s\n%s\n%s\n%s\n%s",
    header,
    paste(params, collapse = "\n"),
    footer,
    add_fields,
    import
  )
}

#' @noRd
prep_tbl_roxy <- function(obj,
                          title = NULL,
                          description = NULL,
                          use_labels = TRUE,
                          add_fields=sinew_opts$get("add_fields"),
                          header_add = sinew_opts$get(),
                          env = parent.frame()) {
  cl <- vapply(obj, typeof, character(1))
  cl_desc <- rep("COLUMN_DESCRIPTION", ncol(obj))
  
  if (use_labels) {
    cl_desc <- as.character(lapply(obj, \(x) {attr(x, "label", exact = TRUE)}))
    cl_desc[cl_desc == "NULL"] <- "COLUMN_DESCRIPTION"
  }
  
  # Write individual item description templates
  items <- paste0(sprintf("#'   \\item{\\code{%s}}{%s %s}", names(cl), cl, cl_desc), collapse = "\n")
  
  header <- set_roxy_header(
    title = title,
    description = description,
    defaults = c("DATASET_TITLE", "DATASET_DESCRIPTION"),
    format = sprintf("#' @format A data frame with %s rows and %s variables:", nrow(obj), length(cl)),
    collapse = "\n"
  )
  
  add_fields <- set_roxy_add_fields(
    add_fields = setdiff(add_fields, c('export','examples','seealso','rdname')),
    header_add = header_add
  ) 
  
  sprintf(
    "%s\n%s\n%s%s",
    header,
    sprintf("#' \\describe{\n%s \n#'}", items),
    add_fields,
    sprintf('\n"%s"', obj_lbl(obj, env = env))
  )
}

#' @noRd
prep_vec_roxy <- function(obj,
                          title = NULL,
                          description = NULL,
                          add_fields=sinew_opts$get("add_fields"),
                          header_add = sinew_opts$get(),
                          env = parent.frame()) {
  obj_type <- typeof(obj)
  
  if (obj_type != "list") {
    obj_type <- paste0(obj_type, " vector")
  }
  
  header <- set_roxy_header(
    title = title,
    description = description,
    defaults = c("DATASET_TITLE", "DATASET_DESCRIPTION"),
    format = sprintf("#' @format A length %s %s.", length(obj), obj_type),
    collapse = "\n"
  )
  
  add_fields <- set_roxy_add_fields(
    add_fields = setdiff(add_fields, c('export','examples','seealso','rdname')),
    header_add = header_add
  ) 
  
  sprintf(
    "%s\n%s%s",
    header,
    add_fields,
    sprintf('\n"%s"', obj_lbl(obj, env = env))
  )
}

#' @noRd
set_roxy_header <- function(title = NULL, description = NULL, ..., defaults = list(), collapse = "\n") {
  if (is.null(title)) {
    title <- defaults[[1]]
  }
  
  if (is.null(description)) {
    description <- defaults[[2]]
  }
  
  header <- c(
    title = paste0("#' @title ", title),
    description = paste0("#' @description ", description),
    ...
  )
  
  paste0(header, collapse = collapse)
}

#' @noRd
set_roxy_add_fields <- function(add_fields = NULL, header_add = sinew_opts$get()) {
  if (is.null(add_fields)) {
    return("")
  }
    paste(
      sprintf(
        "#' @%s %s",
        names(header_add[add_fields]),
        header_add[add_fields]
      ),
      collapse = "\n"
    )
}