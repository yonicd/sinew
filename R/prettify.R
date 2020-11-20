#' @importFrom crayon strip_style
prettify <- function(TXT,force = NULL, ignore = NULL, overwrite = FALSE, sos = FALSE, ask = TRUE,askenv = NULL){
  
  SPATH <- basename(grep('library',searchpaths(),value = TRUE))
  
  NMPATH <- c(SPATH,setdiff(loadedNamespaces(),SPATH))
  
  INST <- .packages(all.available = TRUE)
  
  DYNPATH <- unlist(sapply(library.dynam(), "[", 2))
  
  RET <- sapply(names(TXT), prettify_elem,
                TXT=TXT,
                NMPATH=NMPATH,
                INST=INST,
                DYNPATH=DYNPATH,
                force = force, 
                ignore = ignore, 
                overwrite = overwrite, 
                sos = sos,
                ask = ask,
                askenv = askenv, 
                simplify = FALSE)
  
  RET <- lapply(RET,function(x){
    x$new_text <- crayon::strip_style(string = x$new_text)
    x
  })
  
  if (length(RET) == 1) RET <- RET[[1]]
  
  RET
}

prettify_elem <- function(nm,TXT,NMPATH,INST,DYNPATH,force = NULL, ignore = NULL, overwrite = FALSE, sos = FALSE, ask = TRUE, askenv = NULL) {
  
  txt <- TXT[[nm]]
  
  sym.funs <- pretty_parse(txt, ask)
  
  if (length(sym.funs)==0)
    return(NULL)
  
  if (nrow(sym.funs)==0)
    return(NULL)
  
  sym.funs$namespace <- NA
  
  funs <- sym.funs$text[is.na(sym.funs$namespace)]
  
  if (length(funs)==0)
    return(NULL)
  
  sym.funs <- pretty_find(
    NMPATH = NMPATH,
    sos = sos,
    sym.funs = sym.funs,
    funs = funs,
    txt = txt,
    ask = ask,
    askenv = askenv
  )
  
  pretty_shift(
    txt = txt,
    sym.funs = sym.funs,
    nm = nm,
    overwrite = overwrite,
    force = force,
    ignore = ignore
  )
  
}

pretty_setup <- function(con,text){
  
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
  
  return(TXT)
}