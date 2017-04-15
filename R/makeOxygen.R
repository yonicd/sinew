#' @title Creates skeleton Roxygen with information from within function script
#' @description Creates Roxygen skeleton including title, description, import and other fields
#' @param obj function or name of function
#' @param add_default logical to add defaults values to the end of the PARAM fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen fields, Default: NULL
#' @param print logical write output to console, Default: TRUE
#' @param ... arguments to be passed to makeImport
#' @details add_fields can include (concept,keyword,usage,export,details,examples). the order in add_fields
#' determines the order of printout.
#' @export
#' @examples 
#' makeOxygen(stats::lm,add_default = TRUE,add_fields = c('export','examples'))
makeOxygen=function(obj,add_default=TRUE, add_fields=NULL, print=TRUE, ...){
  
  header_add=c(
    concept ="#' @concept CONCEPT_TERM",
    keyword ="#' @keyword KEYWORD_TERM",
    usage   ="#' @usage USAGE_DESCRIPTION",
    export  ="#' @export",
    details ="#' @details DETAILS",
    examples="#' @examples\n#' EXAMPLE1 \n",
    source  ="#' @source \\url{http://somewhere.important.com/}\n"
  )
  
  lbl=deparse(substitute(obj))
  lbl=gsub('"','',lbl)
  
  if(is.character(obj)) obj=eval(parse(text=obj))
  
  if(inherits(obj,c('data.frame','tibble'))){
      cl <- sapply(obj, typeof)
      
      # Write individual item description templates
      items <- paste0(sprintf("#'   \\item{\\code{%s}}{%s COLUMN_DESCRIPTION}",names(cl),cl), collapse = "\n")
      
      header=c(
               title="#' @title DATASET_TITLE",
               description="#' @description DATASET_DESCRIPTION",
               format=sprintf("#' @format A data frame with %s rows and %s variables:",nrow(obj),length(cl))
              )
      
      ret=sprintf('%s\n%s\n%s%s',
                  paste(header,collapse = '\n'),
                  sprintf("#' \\describe{\n%s \n#'}", items),
                  ifelse(!is.null(add_fields),paste(header_add[add_fields],collapse = '\n'),''),
                  sprintf('"%s"',lbl)
      )
    }
  
  if(inherits(obj,c('function'))){
  
    importList=list(...)
    importList$script=obj
    importList$print=FALSE
    import=do.call('makeImport',importList)
    if(import=='list()') import=''
    
    cutOFF=ifelse('cut'%in%names(importList),importList$cut,3)
    if('seealso'%in%add_fields) header_add=c(header_add,seealso=paste0(makeSeeAlso(obj,cutOFF=cutOFF),'\n',collapse='\n'))
    
    str_out='PARAM_DESCRIPTION'
    
    out=sapply(formals(obj),function(y){
      cl=class(y)
      out=as.character(y)
      if(cl=='NULL') out='NULL'
      if(cl=='character') out=sprintf("'%s'",as.character(y))
      if(cl%in%c('if','call')) out=deparse(y)
      out=paste0(out,collapse ="\n#'")
      if(add_default){
        if(nchar(out)>0){
          out=sprintf(", Default: %s",out)
        }
        str_out=sprintf('PARAM_DESCRIPTION%s',out)
      }
      
      return(str_out)
    })
    
    header=c(title="#' @title FUNCTION_TITLE",
             description="#' @description FUNCTION_DESCRIPTION")

    ret=sprintf('%s\n%s\n%s%s',
                paste(header,collapse = '\n'),
                paste(sprintf("#' @param %s %s",names(out),out),collapse='\n'),
                ifelse(!is.null(add_fields),paste(header_add[add_fields],collapse = '\n'),''),
                import
    )
    
  }
  
  if(print) writeLines(ret)
  
  invisible(ret)

}
