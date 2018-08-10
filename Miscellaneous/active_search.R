#' @importFrom stats na.omit
active_search <- function(funs, INST,NMPATH, DYNPATH, prblm=c('lubridate','emo')){
  
  inst.pkgs <- setdiff(INST,NMPATH)
  
  message('still missing functions: ',paste0(unique(funs),collapse=','),
          ', updating memoised index of other installed libraries (',
          length(inst.pkgs),
          ')... the initial index may be a while\n')
  
  FOUND = c()
  
  while( length(inst.pkgs)>0 ){
    
    this_pkg <- inst.pkgs[1]
    
    # cat(paste0(this_pkg,','))
    
    if(this_pkg=='nothing'){
      inst.pkgs <- inst.pkgs[-stats::na.omit(match(this_pkg,inst.pkgs))]
      next
    }
    
    dep_x <- dep_pkg(this_pkg)
    
    check_x <- setdiff(c(NMPATH,dep_x),loadedNamespaces())
    
    if(any(prblm%in%check_x)){
      #message(this_pkg,' contains: ', paste0(intersect(prblm,check_x),collapse=', '), ' \n')
      inst.pkgs <- inst.pkgs[-stats::na.omit(match(check_x,inst.pkgs))]
      next
    }
    
    suppressMessages({
      suppressWarnings({
        found <- unlist(sapply(check_x,mf,funs,simplify = FALSE))
        
        something(NMPATH)
        
        something.dyn(DYNPATH)
        
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
  
  FOUND
}