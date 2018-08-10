#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param force list, named list of functions to force over the
#'  internal search (seee details), Default: NULL
#' @param ignore list, named list of functions to ignore (seee details), Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param sos boolean, apply sos search for uninstalled libraries, Default: FALSE
#' @return character
#' @details Searches for functions in the loadedNamespace, help.search and then \code{\link[sos]{findFn}}.
#' If force is not NULL but a named list eg list(stats=c('rnorm','runif'),utils = 'head'),
#' then the value pairs will be used in place of what was found using the search path.
#' If ignore is not NULL but a named list eg list(stats=c('rnorm','runif'),utils = 'head'),
#' then if the functions are found they will not have a namespace attached to them.
#' 
#' If you want to toggle off the summary console printing you can set it globally via 
#' sinew_opts$set(pretty_print=FALSE).
#' 
#' @examples
#' txt <- '#some comment
#' yy <- function(a=4){
#'   head(runif(10),a)
#'   # a comment
#' }
#'
#' zz <- function(v=10,a=8){
#'   head(runif(v),a)
#' }'
#'
#' pretty_namespace(text=txt)
#'
#' @seealso
#'  \code{\link[sos]{findFn}}
#'  \code{\link[utils]{help.search}}
#' @rdname pretty_namespace
#' @export
#' @author Jonathan Sidi
pretty_namespace <- function(con = NULL, text = NULL, force = NULL, ignore = NULL, overwrite = FALSE, sos = FALSE) {
  
  if (is.null(text) & is.null(con)) return(NULL)

  if (is.null(text)) {
    
    if (length(con) == 1L && file.info(con)$isdir) {
      
      files <- list.files(path = con, pattern = ".+\\.[rR]$", full.names = TRUE)
      
    } else {
      
      files <- con
      
    }

    TXT <- sapply(files, readLines, warn = FALSE, simplify = FALSE)
    
  } else {
    
    if (length(text) == 1) 
      TXT <- strsplit(text, "\n")

    names(TXT) <- sprintf("txt%s", 1:length(TXT))
  }

  NMPATH <- loadedNamespaces()

  INST <- rownames(installed.packages())

  DYNPATH <- unlist(sapply(library.dynam(), "[", 2))

  RET <- sapply(names(TXT), function(nm) {
    
    txt <- TXT[[nm]]

    sym.funs <- pretty_parse(txt)
    
    if (length(sym.funs)==0)
      return(txt)
    
    if (nrow(sym.funs)==0)
      return(txt)
    
    sym.funs$namespace <- NA

    funs <- sym.funs$text[is.na(sym.funs$namespace)]

    if (length(funs)==0)
      return(txt)
    
    sym.funs <- pretty_find(
      NMPATH = NMPATH,
      sos = sos,
      sym.funs = sym.funs,
      funs = funs
      )

    pretty_shift(
      txt = txt,
      sym.funs = sym.funs,
      nm = nm,
      overwrite = overwrite,
      force = force,
      ignore = ignore
      )

  }, simplify = FALSE)

  if (length(RET) == 1) RET <- RET[[1]]

  invisible(RET)
}
