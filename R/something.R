something <- function(NMPATH){
  
  repeat{
  
  pkgs <- setdiff(loadedNamespaces(),c(NMPATH,'lazyeval'))
  
  if (!length(pkgs)) break
    for (pkg in pkgs) {
      try(unloadNamespace(pkg), silent = TRUE)
    }
  }
  
}

something.dyn <- function(DYNPATH){
  dynpath <- unlist(sapply(library.dynam(),'[',2))
  
  rm.dynpath <- setdiff(dynpath,DYNPATH)
  
  if(length(rm.dynpath)>0){
    
    dyn.junk <- sapply(rm.dynpath,function(x){
      
      path <- gsub('/libs(.*?)$','',x)
      pkg <- basename(path)
      if(!any(pkg%in%c('rlang','lazyeval')))
        try(library.dynam.unload(chname = pkg,libpath = path))
    })
    
  }}