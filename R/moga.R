#' @title Make Oxygen Great Again
#' @description Update/append an R file that has roxygen2 headers already with updated information
#' @param path character path to R file
#' @param \dots arguments to be passed to new makeOxygen
#' @param force.fields character, vector a field names that are in current header that are to be updated Default: NULL
#' @param dry.run boolean, write lines to console the output, Default: TRUE
#' @param overwrite boolean, overwrite contents of input file, Default: FALSE
#' @return character
#' @details Cross references fields already in the roxygen2 header and adds any new ones from the updated call.
#' To force a change to a field add field name to force.fields.
#' @examples
#' 
#' # We want to update the contents of the Roxygen2 with the new parameter "b"
#' # without touching the other fields
#' 
#' # Before
#'  cat(readLines(system.file('example_moga.R',package = 'sinew')),sep = '\n')
#' 
#' # After
#'  moga(system.file('example_moga.R',package = 'sinew'))
#'  
#' @concept populate
#' @rdname moga
#' @export
moga <- function(path, ..., force.fields=NULL, dry.run=TRUE, overwrite=FALSE) {
  l <- readLines(path, warn = FALSE)
  fn_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(path))
  assign(fn_name, eval(parse(text = l[!grepl("^#'", l)])))

  oxy_current <- paste0(grep("^#'", l, value = TRUE), collapse = "\n")

  oxy_new <- c()

  eval(parse(text = sprintf("oxy_new<-makeOxygen(%s,print=FALSE,...)", fn_name), keep.source = TRUE))

  oxy_list <- sapply(c(current = oxy_current, new = oxy_new), get_oxy, simplify = FALSE)

  oxy_current_names <- names(oxy_list[[1]])

  if (!is.null(force.fields)) {
    for (f0 in force.fields) {
      f.val <- grep(f0, oxy_current_names, value = TRUE)
      if (length(f.val) > 0) oxy_list[[1]][f.val] <- oxy_list[[2]][f.val]
    }
  }

  oxy_update <- c(oxy_list[[1]], oxy_list[[2]][setdiff(names(oxy_list[[2]]), names(oxy_list[[1]]))])

  oxy_update <- gsub("^#'", "\n#'", oxy_update)

  oxy_update <- Filter(Negate(is.na), oxy_update)

  idx <- unlist(sapply(names(oxy_update), function(x) {
    out <- grep(gsub("\\s(*.?)$", "", x), names(oxy_list[[1]]))
    if (length(out) == 0) out <- length(names(oxy_update)) + 1
    out[1]
  }))

  oxy_out <- sprintf("#' @%s %s", names(sort(idx)), oxy_update[names(sort(idx))])

  if (dry.run) {
    writeLines(oxy_out)
  }

  if (overwrite) {
    idx <- grep("^#'", l)
    if (length(idx) > 0) {
      if (length(idx) > 1) l <- l[-idx[-1]]
      l[idx[1]] <- paste0(oxy_out, collapse = "\n")
    }
    cat(l, file = path, append = FALSE, sep = "\n")
  }

  invisible(oxy_out)
}
