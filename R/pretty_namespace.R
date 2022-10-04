#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param ask boolean, If TRUE then a \code{\link[utils]{menu}} will be created for the use to
#'  choose between competing namespaces for a function, Default: TRUE
#' @param askenv environment, environment that stores names of functions to force in ask,
#'  Default: new.env()
#' @param force list, named list of functions to force over the
#'  internal search (seee details), Default: NULL
#' @param ignore list, named list of functions to ignore (seee details), Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param sos boolean, apply sos search for uninstalled libraries, Default: FALSE
#' @return character
#' @details Searches for functions in the \code{loadedNamespace}, \code{help.search} and then \code{\link[sos]{findFn}}.
#' If force is not NULL but a named list eg \code{list(stats=c('rnorm','runif'),utils = 'head')},
#' then the value pairs will be used in place of what was found using the search path.
#' If ignore is not NULL but a named list eg \code{list(stats=c('rnorm','runif'),utils = 'head')},
#' then if the functions are found they will not have a namespace attached to them.
#' 
#' If you want to toggle off the summary console printing you can set it globally via 
#' \code{sinew_opts$set(pretty_print=FALSE)}.
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
#' @seealso [findFn][sos::findFn], [help.search][utils::help.search]
#' @rdname pretty_namespace
#' @concept namespace
#' @export
#' @author Jonathan Sidi
pretty_namespace <- function(con = NULL,
                             text = NULL,
                             ask = TRUE,
                             askenv = new.env(),
                             force = NULL,
                             ignore = NULL,
                             overwrite = FALSE,
                             sos = FALSE) {
  
  validate_force(force)
  
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
  
  RET <- prettify(TXT, force, ignore, overwrite, sos, ask, askenv)

  invisible(RET)
}





#' @title Create lists of `package` exports 
#' @description Useful for supplying packages to the `force` argument to `pretty_namespace`.
#' @param packages \code{(character)} packages to include in list. When duplicate function names exist the order of packages determines which function will be selected first - IE the first package with the function name will include that function, the second package with the function name will not have it listed.
#'
#' @return \code{(named list)} with package names as names as all exports as a character vector
#' @export
#'
#' @examples
#' make_force_packages(c("utils"))
#' @rdname make_force_packages

make_force_packages <- function (packages) {
  names(packages) <- packages
  force <- 
    lapply(packages, function(.x) getNamespaceExports(.x)[!grepl("^\\.", getNamespaceExports(.x))])
  if (length(force) > 1) {
    end_idx <- ifelse(length(force) == 1, 1, length(force) - 1)
    # Use first package
    for (idx in 1:end_idx) {
      pkg <- force[[idx]]
      for (fn in pkg) {
        for (p_idx in (idx + 1):length(force)) {
          force[[p_idx]] <- force[[p_idx]][!grepl(paste0("^",fn,"$"), force[[p_idx]])]
        }
      }
    }
  }
  
  force
}
