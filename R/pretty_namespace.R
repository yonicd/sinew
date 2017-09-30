#' @title Append namespace to functions in script
#' @description Autoappend namespace to functions in script by searchpath order
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param check.installed boolean, check installed.packages for functions, Default: FALSE
#' @param cache_path character, Directory in which to store cached items, Default: tempdir()
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
#' @importFrom memoise memoise
#' @importFrom stats na.omit
#' 
pretty_namespace <- function( con = NULL ,text= NULL, overwrite = FALSE , check.installed = FALSE, cache_path = tempdir()){

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
  
  f <- function(x,pat){
    ns <- try(
      {ls(envir=asNamespace(x),pattern = sprintf('^(%s)$',paste0(pat,collapse='|')))},
      silent = TRUE)
    
    if(class(ns)=="try-error") 
      ns <- vector('character')
    
    ns
  }
  
  mf <- memoise::memoise(f,cache = memoise::cache_filesystem(cache_path))
  
  f1 <- function(this_pkg){ c(this_pkg,
                              tools::package_dependencies(this_pkg,
                                                          which = c('Depends','Imports'),
                                                          recursive = TRUE)[[1]])}
  
  dep_pkg <- memoise::memoise(f1,cache = memoise::cache_filesystem(cache_path))
  
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
    
    for(x in NMPATH){
      
      if(length(funs)==0) break
    
      found <- funs%in%mf(x,funs)
      
      sym.funs$namespace[sym.funs$text%in%funs[found]] <- x
      
      funs <- funs[!found]
      
    }
    
    
    if(check.installed){
    if( length(funs)>0 ){
      
      inst.pkgs <- setdiff(INST,NMPATH)
      
      message('still missing functions: ',paste0(unique(funs),collapse=','),
              ', updating memoised index of other installed libraries (',
              length(inst.pkgs),
              ')... the initial index may be a while\n')
      
      prblm=c('lubridate')
      
      FOUND = c()
      
      while( length(inst.pkgs)>0 ){
        
        this_pkg <- inst.pkgs[1]
        
        if(this_pkg=='nothing'){
          inst.pkgs <- inst.pkgs[-stats::na.omit(match(this_pkg,inst.pkgs))]
          next
        }
        
        dep_x <- dep_pkg(this_pkg)
        
        check_x <- setdiff(c(NMPATH,dep_x),loadedNamespaces())
        
        if(prblm%in%check_x){
          #message(this_pkg,' contains: ', paste0(intersect(prblm,check_x),collapse=', '), ' \n')
          inst.pkgs <- inst.pkgs[-stats::na.omit(match(check_x,inst.pkgs))]
          next
        }
        
        suppressMessages({
          suppressWarnings({
            found <- unlist(sapply(check_x,mf,funs,simplify = FALSE))
            
            something(c(NMPATH,'lazyeval'))
            
            dynpath <- unlist(sapply(library.dynam(),'[',2))
            
            rm.dynpath <- setdiff(dynpath,DYNPATH)
            
            if(length(rm.dynpath)>0){
              
              dyn.junk <- sapply(rm.dynpath,function(x){
                
                path <- gsub('/libs(.*?)$','',x)
                pkg <- basename(path)
                
                library.dynam.unload(chname = pkg,libpath = path)   
              })
              
            }
            
          })
        })
        
        if(length(found)>0){
          FOUND <- c(FOUND,found)
          funs <- funs[-match(found,funs)]
        }
        
        if(length(funs)==0){
          message('found all!')
          break 
        }
        
        inst.pkgs <- inst.pkgs[-stats::na.omit(match(check_x,inst.pkgs))]
        
        #cat(paste0(length(inst.pkgs),','))
        
      }
     
      if(length(FOUND)){
        for(i in 1:length(FOUND)){
          sym.funs$namespace[match(FOUND[i],sym.funs$text)] <- names(FOUND[i])    
        }
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
