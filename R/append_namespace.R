#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param text character, vector that contains script, Default: NULL
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @return character
#' @details searches for functions in the loadedNamespace and then in the remaining installed.packages
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
#' append_namespace(text=txt)
#' @seealso 
#'  \code{\link[stringi]{stri_sub}}
#'  \code{\link[utils]{getParseData}}
#' @rdname append_namespace
#' @export 
#' @author Jonathan Sidi
#' @importFrom stringi stri_sub
#' @importFrom utils getParseData
append_namespace <- function(text= NULL, con = NULL , overwrite = FALSE){

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
  
  NMPATH <- loadedNamespaces()

  INST <- rownames(installed.packages())

  RET <- sapply(names(TXT), function(nm){

    txt <- TXT[[nm]]
    
    p <- parse(text = txt)
    
    p1 <- utils::getParseData(p)
    
    rmParent <- p1$parent[p1$token=='SYMBOL_PACKAGE']
    
    sym.funs <- p1[p1$token=='SYMBOL_FUNCTION_CALL'&!p1$parent%in%rmParent,]
    
    sym.funs$namespace <- NA
    
    funs <- sym.funs$text[is.na(sym.funs$namespace)]
    
    for(x in NMPATH){
      
      if(length(funs)==0) break
      
      found <- funs%in%ls(envir=asNamespace(x))
      
      sym.funs$namespace[sym.funs$text%in%funs[found]] <- x
      
      funs <- funs[!found]
      
      
    }
    
    if( length(funs)>0 ){
      
      inst.pkgs <- INST[!INST%in%NMPATH]
      
      for( x in inst.pkgs ){
        
        if(length(funs)==0) break
        
        found <- funs%in%ls(envir=asNamespace(x))
        
        sym.funs$namespace[sym.funs$text%in%funs[found]] <- x
        
        funs <- funs[!found]
      }
      
    }
    
    sym.funs$new_text <- paste(sym.funs$namespace,sym.funs$text,sep='::')
    
    idx <- which(!sym.funs$namespace%in%'base')
    
    for(ii in 1:length(idx)){
      
      i <- idx[ii]
      
     if( ii>1 ){
       if( (sym.funs$line1[i]-sym.funs$line1[i-1])==0 ){
        sym.funs$col1[i] <- sym.funs$col1[i]+(nchar(sym.funs$new_text[i-1])-nchar(sym.funs$text[i-1]))
        sym.funs$col2[i] <- sym.funs$col2[i]+(nchar(sym.funs$new_text[i-1])-nchar(sym.funs$text[i-1]))
       }
     }
      
      stringi::stri_sub(txt[sym.funs$line1[i]], sym.funs$col1[i], sym.funs$col2[i]) <- sym.funs$new_text[i]
    }

    if(overwrite){
      cat(txt,sep='\n',file = nm) 
    }else{
      writeLines(txt)
    }
    
    txt
  
  })
  
  invisible(RET)
  
}
