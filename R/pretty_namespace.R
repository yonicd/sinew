#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param check.installed boolean, check installed.packages for functions, Default: FALSE
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
#' pretty_namespace(text=txt)
#' 
#' @seealso 
#'  \code{\link[stringi]{stri_sub}}
#'  \code{\link[utils]{getParseData}}
#' @rdname pretty_namespace
#' @export 
#' @author Jonathan Sidi
#' @importFrom stringi stri_sub
#' @importFrom utils getParseData setTxtProgressBar txtProgressBar
#' 
pretty_namespace <- function( con = NULL ,text= NULL, overwrite = FALSE , check.installed = FALSE){

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
    
    
    if(check.installed){
    if( length(funs)>0 ){
      
      prblm <- c('bsplus','lubridate')
      
      inst.pkgs <- INST[!INST%in%c(NMPATH,prblm)]
    
      message('still missing functions: ',paste0(unique(funs),collapse=','),
              ', looking in other installed libraries (',
              length(inst.pkgs),
              ')... this could be a while')
      
      pb <- txtProgressBar(min = 1, max = length(inst.pkgs), initial = 1, style = 3)
      
      suppressMessages({
        suppressWarnings({
      
      for( x in inst.pkgs ){
        
        cat(paste0(x,','))
        
        if(length(funs)==0){
          message('found all!')
          break 
        }
        
        ns <- try(asNamespace(x),silent = TRUE)
        
        if(class(ns)!="try-error")
          found <- funs%in%ls(envir=ns)

        something(NMPATH)
       
        sym.funs$namespace[sym.funs$text%in%funs[found]] <- x
        
        funs <- funs[!found]
        
        setTxtProgressBar(pb, match(x,inst.pkgs))
      }
      
      close(pb)
      
      })
    })
      
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
  
  })
  
  invisible(RET)
  
}
