#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param sos boolean, apply sos search for uninstalled libraries, Default: FALSE
#' @return character
#' @details searches for functions in the loadedNamespace, help.search and then \code{\link[sos]{findFn}}
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
#' @importFrom stringi stri_sub
#' @importFrom sos findFn
#' @importFrom utils help.search
pretty_namespace <- function(con = NULL, text= NULL, overwrite = FALSE, sos = FALSE) {
  if (is.null(text) & is.null(con)) return(NULL)

  if (is.null(text)) {
    if (length(con) == 1L && file.info(con)$isdir) {
      files <- list.files(path = con, pattern = ".+\\.[rR]$", full.names = TRUE)
    } else {
      files <- con
    }

    TXT <- sapply(files, readLines, warn = FALSE, simplify = FALSE)
  } else {
    if (length(text) == 1) TXT <- strsplit(text, "\n")

    names(TXT) <- sprintf("txt%s", 1:length(TXT))
  }

  mf <- function(x, pat) {
    ns <- try(
      {
        
        if(!isNamespaceLoaded(x)){
          y <- attachNamespace(x)  
        }
        
        ls(envir = sprintf('pacakge:%s',x), pattern = sprintf("^(%s)$", paste0(pat, collapse = "|")))
      },
      silent = TRUE
    )

    if (class(ns) == "try-error") {
      ns <- vector("character")
    }

    ns
  }

  NMPATH <- loadedNamespaces()

  INST <- rownames(installed.packages())

  DYNPATH <- unlist(sapply(library.dynam(), "[", 2))

  RET <- sapply(names(TXT), function(nm) {
    txt <- TXT[[nm]]

    p <- parse(text = txt)

    p1 <- utils::getParseData(p)

    rmParent <- p1$parent[p1$token == "SYMBOL_PACKAGE"]

    sym.funs <- p1[p1$token == "SYMBOL_FUNCTION_CALL" & !p1$parent %in% rmParent, ]

    if (length(sym.funs)==0)
      return(txt)
    
    if (nrow(sym.funs)==0)
      return(txt)
    
    sym.funs$namespace <- NA

    funs <- sym.funs$text[is.na(sym.funs$namespace)]

    if (length(funs)==0)
      return(txt)
    
    check_global <- ls(envir = get(search()[1]))
    
    if (length(check_global)>0){
      global.funs <- check_global[sapply(check_global, function(x) class(get(x)) == "function")]
      funs <- funs[!funs %in% global.funs]  
    }

    for (x in NMPATH) {
      if (length(funs) == 0) break

      found <- funs %in% mf(x, funs)

      sym.funs$namespace[sym.funs$text %in% funs[found]] <- x

      funs <- funs[!found]
    }

    if (length(funs) > 0) {
      for (fun in funs) {
        suppressWarnings(fun.help <- utils::help.search(sprintf("^%s$", fun), ignore.case = FALSE))
        if (nrow(fun.help$matches) > 0) {
          sym.funs$namespace[sym.funs$text %in% fun] <- fun.help$matches$Package[1]
          funs <- funs[-match(fun, funs)]
        }
      }
    }

    if (sos & length(funs) > 0) {
      for (fun in funs) {
        suppressWarnings(fun.sos <- sos::findFn(fun, maxPages = 1, verbose = 0))
        if (nrow(fun.sos)) {
          sym.funs$namespace[sym.funs$text %in% fun] <- fun.sos$Package[1]
          funs <- funs[-match(fun, funs)]
        }
      }
    }

    sym.funs$new_text <- paste(sym.funs$namespace, sym.funs$text, sep = "::")

    idx <- which(!sym.funs$namespace %in% c("base", NA))

    sym.funs.i <- split(sym.funs[idx,],sym.funs$line1[idx])
    
    sym.funs.i.shift <- lapply(sym.funs.i,function(sf){
      
      x <- rep(0,nrow(sf))
      
      if(nrow(sf)>1){
        for(i in 2:nrow(sf)){
          
          x[i:nrow(sf)] <- x[i] + (nchar(sf$new_text[i - 1]) - nchar(sf$text[i - 1]))
          
        }
      }
      
      sf$col_shift <- x
      
      sf
    })
    
    sym.funs.shift <- do.call('rbind',sym.funs.i.shift)
    
    sym.funs.shift$col1 <- sym.funs.shift$col1 + sym.funs.shift$col_shift
    sym.funs.shift$col2 <- sym.funs.shift$col2 + sym.funs.shift$col_shift
    
    for(i in 1:length(idx)){
      stringi::stri_sub(
        str = txt[sym.funs.shift$line1[i]],
        from = sym.funs.shift$col1[i],
        to = sym.funs.shift$col2[i]) <- sym.funs.shift$new_text[i]  
    }
    
    if (overwrite) {
      cat(txt, sep = "\n", file = nm)
    } else {
      writeLines(txt)
    }

    if (length(funs)) message("Not Found: ", paste0(unique(funs), collapse = ","))

    txt
  }, simplify = FALSE)

  if (length(RET) == 1) RET <- RET[[1]]

  invisible(RET)
}
