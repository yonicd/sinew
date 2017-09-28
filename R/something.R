something <- function(NMPATH){
  
  repeat{
  
  pkgs <- setdiff(loadedNamespaces(),NMPATH)
  
  if (!length(pkgs)) break
    for (pkg in pkgs) {
      try(unloadNamespace(pkg), silent = TRUE)
    }
  }
  
}