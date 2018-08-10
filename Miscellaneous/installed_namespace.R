library(sinew)

NMPATH <- loadedNamespaces()

INST <- rownames(installed.packages())

f <- function(x,pat=funs){
  ns <- try(
              {ls(envir=asNamespace(x),pattern = sprintf('^(%s)$',paste0(pat,collapse='|')))},
          silent = TRUE)
  
  if(class(ns)=="try-error") 
    ns <- vector('character')
    
  ns
}

mf <- memoise::memoise(f)

f1 <- function(this_pkg){ c(this_pkg,
  tools::package_dependencies(this_pkg,
                              which = c('Depends','Imports'),
                              recursive = TRUE)[[1]])}

dep_pkg <- memoise::memoise(f1)

inst.pkgs <- setdiff(INST,NMPATH)

funs <- c('gitbook','buildUsepackage')

#pllogis

prblm=c('lubridate')

FOUND = c()

while( length(inst.pkgs)>0 ){
  
  this_pkg <- inst.pkgs[1]
  
  if(this_pkg=='nothing'){
    inst.pkgs <- inst.pkgs[-na.omit(match(this_pkg,inst.pkgs))]
    next
  }
  
  dep_x <- dep_pkg(this_pkg)
  
  check_x <- setdiff(c(NMPATH,dep_x),loadedNamespaces())
  
  if(prblm%in%check_x){
    message(this_pkg,' contains: ', paste0(intersect(prblm,check_x),collapse=', '), ' \n')
    inst.pkgs <- inst.pkgs[-na.omit(match(check_x,inst.pkgs))]
    next
  }
  
  suppressMessages({
    suppressWarnings({
      found <- unlist(sapply(check_x,mf,funs,simplify = FALSE))
      
      sinew:::something(NMPATH)      
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
  
  inst.pkgs <- inst.pkgs[-na.omit(match(check_x,inst.pkgs))]

  cat(paste0(length(inst.pkgs),','))

}
