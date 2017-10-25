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
pretty_namespace <- function(con = NULL ,text= NULL, overwrite = FALSE, sos = FALSE){

  if(is.null(text)&is.null(con)) return(NULL)
  
  if(is.null(text)){
  
   if (length(con) == 1L && file.info(con)$isdir) {

    files <- list.files(path = con, pattern = ".+\\.[rR]$", full.names = TRUE)

  } else {
    
    files <- con
    
  }
  
   TXT <- sapply(files,readLines, warn = FALSE,simplify = FALSE)    
    
  }else{
    
    if(length(text)==1) TXT <- strsplit(text,'\n')
    
    names(TXT) <- sprintf('txt%s',1:length(TXT))
    
  }
  
  mf <- function(x,pat){
    ns <- try(
      {ls(envir=asNamespace(x),pattern = sprintf('^(%s)$',paste0(pat,collapse='|')))},
      silent = TRUE)

    if(class(ns)=="try-error")
      ns <- vector('character')

    ns
  }

  NMPATH <- loadedNamespaces()

  INST <- rownames(installed.packages())
  
  DYNPATH <- unlist(sapply(library.dynam(),'[',2))
  
  RET <- sapply(names(TXT), function(nm){

    txt <- TXT[[nm]]
    
    p <- parse(text = txt)
    
    p1 <- utils::getParseData(p)
    
    rmParent <- p1$parent[p1$token=='SYMBOL_PACKAGE']
    
    sym.funs <- p1[p1$token=='SYMBOL_FUNCTION_CALL'&!p1$parent%in%rmParent,]
    
    sym.funs$namespace <- NA
    
    funs <- sym.funs$text[is.na(sym.funs$namespace)]
    
    global.funs <- ls(envir = sys.frame(-4))[sapply(ls(envir = sys.frame(-4)),function(x) class(get(x))=='function')]

    funs <- funs[!funs%in%global.funs]
      
    for(x in NMPATH){
      
      if(length(funs)==0) break
    
      found <- funs%in%mf(x,funs)
      
      sym.funs$namespace[sym.funs$text%in%funs[found]] <- x
      
      funs <- funs[!found]
      
    }
    
    if( length(funs)>0 ){
      for(fun in funs){
        suppressWarnings(fun.help <- utils::help.search(sprintf('^%s$',fun),ignore.case = FALSE))
        if(nrow(fun.help$matches)>0){
          sym.funs$namespace[sym.funs$text%in%fun] <- fun.help$matches$Package[1]
          funs <- funs[-match(fun,funs)]
        }
      }
    }
    
    if( sos & length(funs)>0 ){
      for(fun in funs){
        suppressWarnings(fun.sos <- sos::findFn(fun,maxPages = 1,verbose = 0))
        if(nrow(fun.sos)){
          sym.funs$namespace[sym.funs$text%in%fun] <- fun.sos$Package[1]
          funs <- funs[-match(fun,funs)]
        }
      }
    }

    sym.funs$new_text <- paste(sym.funs$namespace,sym.funs$text,sep='::')
    
    idx <- which(!sym.funs$namespace%in%c('base',NA))
    
    for(ii in 1:length(idx)){
      
      i <- idx[ii]
      
     if( ii>1 ){
       
       i1 <- idx[ii-1]
       
       if( (sym.funs$line1[i]-sym.funs$line1[i1])==0 ){
        sym.funs$col1[i] <- sym.funs$col1[i]+(nchar(sym.funs$new_text[i1])-nchar(sym.funs$text[i1]))
        sym.funs$col2[i] <- sym.funs$col2[i]+(nchar(sym.funs$new_text[i1])-nchar(sym.funs$text[i1]))
       }
       
     }
      
      stringi::stri_sub(txt[sym.funs$line1[i]], sym.funs$col1[i], sym.funs$col2[i]) <- sym.funs$new_text[i]
      
    }

    if(overwrite){
      cat(txt,sep='\n',file = nm) 
    }else{
      writeLines(txt)
    }
    
    if(length(funs)) message('Not Found: ',paste0(unique(funs),collapse=','))
    
    txt
  
  },simplify = FALSE)
  
  if(length(RET)==1) RET <- RET[[1]]
  
  invisible(RET)
  
}
