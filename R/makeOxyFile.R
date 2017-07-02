#' @title Inserts roxygen2 skeletons in file(s).
#' @description Applies \code{makeOxygen} function to all functions/dataframes in supplied file(s)
#' @param input character vector of path(s) to one or more .R files, a path to directory containing .R files, Default: NULL, opens system dialogue for files selection
#' @param overwrite When TRUE overwrites file(s), FALSE writes "Oxy"- prefixed files in the same directory, Default: FALSE
#' @param ... additional parameters passed to \code{makeOxygen}
#' @return Nothing. Writes files with roxygen2 comments as a side effect
#' @author Anton Grishin
#' @details If an object cannot be found, asks to sourse the file.
#' Objects will be sourced into a temporary environment.
#' If the file already contains roxygen2 comments they will be deleted to avoid duplication.
#' Some functions may require attaching additional packages. For instance, if functions
#' were defined with purrr's \code{compose} or \code{partial} functions, omission of \code{purr::} in definitions will
#' require \code{library(purrr)} before proceding with \code{makeOxyFile}. 
#' @examples 
#'  makeOxyFile() # default args, opens system file selection dialogue
#'  makeOxyFile("./myRfunctions/utils.R") # on one R file
#'  makeOxyFile("./myRfunctions/") # on all R files in directory
#'  myfields <- c("concept", "describeIn")
#'  makeOxyFile("./myRfunctions/utils.R", add_fields = myfields, cut = 5) # adds more fields to deafaults, passes "cut" to makeImport
#' @export 
#' @seealso \code{\link{makeOxygen}}

makeOxyFile <- function(input = NULL, overwrite = FALSE, ...) {
  
  if (is.null(input)) {input <- file.choose() 
  }    
  if (length(input) == 1L && file.info(input)$isdir) {
    files <- list.files(path = input, pattern = ".+\\.R$", full.names = TRUE)
    files <- grep("/(?!Oxy)\\w+\\.R$", files, perl = TRUE, value = TRUE)
  } else {files <- input}
  
  if (!all(grepl("\\.R$", basename(files)))) {
    stop("Supplied file(s) is not an .R file!", call. = FALSE)
  }
  append_to_lines <- function(.id, .str){
    unlist(append(.str, strsplit(oxy_lst[[which(ins_id == .id)]],
                                 split = "\n", fixed = TRUE),
                  after = .id))
  }
  neg_msg <- "No functions or data frames found in\n"
  for (FILE in files) {
    lines <- readLines(FILE)
    lines <- lines[!grepl("^\\s*#'", lines)]
    objs <- gsub("\\s*([[:alnum:]._]+).*",
                 "\\1", grep("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines, value = TRUE))
    if (length(objs) == 0L) {stop(neg_msg, normalizePath(FILE), call. = FALSE)}
    if (!all(objs %in% ls(envir = parent.frame()))) {
      message(
        paste0("makeOxygen() can't fine some objects in the global environment",
               "\nwould you like to source ", basename(FILE), " file?")
      )
      usr_inp <- readline("y/n ")
      if (usr_inp == "y") {
        nenv <- attach(NULL, name = "makeOxyFile_tempenv")
        on.exit(if ("makeOxyFile_tempenv" %in% search()) {
                                                detach("makeOxyFile_tempenv")
          })
        sys.source(FILE, nenv, keep.source = TRUE)
      } else {
        stop("File parsing aborted by user!", call. = FALSE)
      }
    }
    sel0 <- seq_along(objs)
    if ("makeOxyFile_tempenv" %in% search()) {
      sel0 <- objs %in% ls("makeOxyFile_tempenv")
      objs <- objs[sel0]
    }
    sel_obj <- vapply(objs, function(x){
      inherits(get(x), c("data.frame", "function"))},
      FUN.VALUE = logical(1))
    if (!any(sel_obj)) {warning(neg_msg, normalizePath(FILE))}
    objs <- objs[sel_obj]
    oxy_lst <- lapply(objs, makeOxygen, ...)
    ins_id <- which(grepl("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines)) - 1L
    ins_id <- ins_id[sel0]
    ins_id <- ins_id[sel_obj]
    
    for (i in rev(ins_id)) {lines <- append_to_lines(i, lines)}
    new_name <- if (overwrite) {FILE} else {
                                  paste0(dirname(FILE), "/Oxy", basename(FILE))}
    writeLines(lines, new_name)
    detach("makeOxyFile_tempenv")
  }
  oxyfiles <- if (overwrite) {files} else {
    paste0(dirname(files), "/Oxy", basename(files))}
  if (length(input) > 0L) {
    if (rstudioapi::isAvailable()) {
      for (i in oxyfiles) rstudioapi::navigateToFile(i)} else {file.show(oxyfiles)}
    
    message("File(s) with roxygen2 comment templates have been written to:\n",
            paste0(normalizePath(oxyfiles, winslash = "/"), collapse = "\n")
    )
  }
}
