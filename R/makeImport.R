#' @title Scrape R script to create namespace calls for R documentation
#' @description Scrape r script to create namespace calls for roxygen2, namespace or description files
#' @param script character connection to pass to readLines, can be file path, directory path, url path
#' @param cut integer number of functions to write as importFrom until switches to import, Default: NULL
#' @param print boolean print output to console, Default: TRUE
#' @param format character the output format must be in c('oxygen','namespace','description'), Default: 'oxygen'
#' @examples 
#' makeImport(list.files('R',full.names = TRUE),format = 'oxygen')
#' makeImport(list.files('R',full.names = TRUE),format = 'namespace')
#' makeImport(list.files('R',full.names = TRUE),format = 'description')
#' @export
#' @importFrom utils installed.packages capture.output
makeImport=function(script,cut=NULL,print=TRUE,format='oxygen'){
  rInst<-paste0(row.names(utils::installed.packages()),'::')
  
  if(inherits(script,'function')){
    file<-tempfile()
    utils::capture.output(print(script),file = file) 
  }else{
    file=script
  }
  
  pkg=sapply(file,function(f){
  x<-readLines(f,warn = F)
  x=gsub('^\\s+','',x)
  x=x[!grepl('^#',x)]
  x=x[!grepl('^<',x)]
  s0=sapply(paste0('\\b',rInst),grep,x=x,value=TRUE)
  s1=s0[which(sapply(s0,function(y) length(y)>0))]
  names(s1)=gsub('\\\\b','',names(s1))
  ret=sapply(names(s1),function(nm){
    out=unlist(lapply(s1[[nm]],function(x){
      y=gsub('[\\",\\(\\)]','',unlist(regmatches(x,gregexpr(paste0(nm,'(.*?)[\\)\\(,]'),x))))
      names(y)=NULL
      if(any(y%in%paste0(nm,c('"',"'")))) y=NULL
      y 
    }))
    out=gsub('\\$.*','',out)
    out=unique(out)
    
    if(format=='oxygen'){
      ret=paste0("#' @importFrom ",gsub('::',' ',nm),gsub(nm,'',paste(unique(out),collapse = ' ')))
      if(!is.null(cut)){
        if(length(out)>=cut) ret=paste0("#' @import ",gsub('::','',nm))
      } 
      out=ret
    }
    return(out)
  })
  if(format=='oxygen'){
    if(print) writeLines(paste(' ',f,paste(ret,collapse='\n'),sep = '\n')) 
  }
  return(ret)
  })

  if(format=='oxygen') ret=pkg
  
  if(format=='namespace'){
    pkg=sort(unique(unlist(pkg)))
    pkgN=gsub(':.*','',pkg)
    pkgC=table(pkgN)

    ret=paste0('importFrom(',gsub('::',',',pkg),')')
    
    if(!is.null(cut)){
      ret=sapply(names(pkgC),function(x){
        if(pkgC[x]>=cut){
          sprintf('import(%s)',unique(gsub(':.*','',x))) 
        }else{
          paste0('importFrom(',gsub('::',',',grep(x,pkg,value=T)),')')
        }
      })
    }
    retWrite=paste(' ',paste(unlist(ret),collapse='\n'),sep = '\n')
    if(print) writeLines(retWrite)
    } 
    
  if(format=='description'){
    ret=unique(gsub('::(.*?)$','',unlist(pkg)))
    retWrite=sprintf('Imports: %s',paste(ret,collapse=','))
    if(print) writeLines(retWrite)
  }
  
  if(inherits(script,'function')) unlink(file)
  
  invisible(paste0(ret,collapse = '\n'))
}
