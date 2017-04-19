#' @title Creates skeleton Roxygen with information from within function script
#' @description Creates Roxygen skeleton including title, description, import and other fields
#' @param obj function or name of function
#' @param add_default boolean to add defaults values to the end of the PARAM fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen fields, Default: NULL
#' @param use_dictionary character, path_to_dictionary, Default: NULL
#' @param print boolean print output to console, Default: TRUE
#' @param ... arguments to be passed to makeImport
#' @details add_fields can include any slot except for the defaults (title,description,param,return). 
#' The order in add_fields determines the order of printout. The roxygen fields to add are list below, for more information goto
#' \href{https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html}{Generating Rd files}
#' \tabular{ll}{
#' \strong{Field}    \tab \strong{Skeleton}                           \cr
#' author            \tab AUTHOR [AUTHOR_2]                           \cr
#' backref           \tab src/filename.cpp                            \cr
#' concept           \tab CONCEPT_TERM_1 [CONCEPT_TERM_2]             \cr
#' describeIn        \tab FUNCTION_NAME DESCRIPTION                   \cr
#' details           \tab DETAILS                                     \cr
#' example           \tab path/relative/to/packge/root                \cr
#' export            \tab                                             \cr
#' family            \tab FAMILY_TITLE                                \cr
#' field             \tab FIELD_IN_S4_RefClass DESCRIPTION            \cr
#' format            \tab DATA_STRUCTURE                              \cr
#' importClassesFrom \tab PKG CLASS_a [CLASS_b]                       \cr
#' importMethodsFrom \tab PKG METHOD_a [METHOD_b]                     \cr
#' include           \tab FILENAME.R [FILENAME_b.R]                   \cr
#' inherit           \tab [PKG::]SOURCE_FUNCTION [FIELD_a FIELD_b]    \cr
#' inheritDotParams  \tab [PKG::]SOURCE_FUNCTION                      \cr
#' inheritSection    \tab [PKG::]SOURCE_FUNCTION [SECTION_a SECTION_b]\cr
#' keywords          \tab KEYWORD_TERM                                \cr
#' name              \tab NAME                                        \cr
#' rdname            \tab FUNCTION_NAME                               \cr
#' references        \tab BIB_CITATION                                \cr
#' section           \tab SECTION_NAME                                \cr
#' source            \tab \\url\{http://somewhere.important.com/\}    \cr
#' slot              \tab SLOTNAME DESCRIPTION                        \cr
#' template          \tab FILENAME                                    \cr
#' templateVar       \tab NAME VALUE                                  \cr
#' useDynLib         \tab PKG [routine_a routine_b]               
#'}
#' @export
#' @examples 
#' makeOxygen(stats::lm,add_default = TRUE,add_fields = c('export','examples'))
makeOxygen=function(obj,add_default=TRUE, add_fields=NULL,use_dictionary=NULL, print=TRUE, ...){
  
  header_add=c(
    author            ="AUTHOR [AUTHOR_2]",
    backref           ="src/filename.cpp",
    concept           ="CONCEPT_TERM_1 [CONCEPT_TERM_2]",
    describeIn        ="FUNCTION_NAME DESCRIPTION",
    details           ="DETAILS",
    #evalRd           ="",
    example           ="path_to_file/relative/to/packge/root",
    examples          ="\n#' EXAMPLE1 \n#'",
    export            ="",
    #exportClass      ="",
    #exportMethod     ="",
    family            ="FAMILY_TITLE",
    field             ="FIELD_IN_S4_RefClass DESCRIPTION",
    format            ="DATA_STRUCTURE",
    importClassesFrom ="PKG CLASS_a [CLASS_b]",
    importMethodsFrom ="PKG METHOD_a [METHOD_b]",
    include           ="FILENAME.R [FILENAME_b.R]",
    inherit           ="[PKG::]SOURCE_FUNCTION [FIELD_a FIELD_b]",
    inheritDotParams  ="[PKG::]SOURCE_FUNCTION",
    inheritSection    ="[PKG::]SOURCE_FUNCTION [SECTION_a SECTION_b]",
    keywords          ="KEYWORD_TERM",
    name              ="NAME",
    #note             ="",
    #noRd             ="",
    #rawRd            ="",
    #rawNamespace     ="",
    rdname            ="FUNCTION_NAME",
    references        ="BIB_CITATION",
    section           ="SECTION_NAME",
    source            ="\\url{http://somewhere.important.com/}",
    slot              ="SLOTNAME DESCRIPTION",
    template          ="FILENAME",
    templateVar       ="NAME VALUE",
    useDynLib         ="PKG [ROUTINE_a ROUTINE_b]"
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
    if('seealso'%in%add_fields) header_add=c(header_add,seealso=paste0(makeSeeAlso(obj,cutOFF=cutOFF),collapse='\n'))
    
    str_out='PARAM_DESCRIPTION'
    
    if(is.null(use_dictionary)){
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
      params=sprintf("#' @param %s %s",names(out),out)
    }else{
      params=ls_param(obj=obj,dictionary = use_dictionary,print = FALSE)
    }
    
    header=c(title="#' @title FUNCTION_TITLE",
             description="#' @description FUNCTION_DESCRIPTION")

    footer=c(return="#' @return OUTPUT_DESCRIPTION")

    ret=sprintf('%s\n%s\n%s\n%s\n%s',
                paste(header,collapse = '\n'),
                paste(params,collapse='\n'),
                footer,
                ifelse(!is.null(add_fields),{
                  paste(sprintf("#' @%s %s",
                                names(header_add[add_fields]),
                                header_add[add_fields]),
                        collapse = '\n')
                },''),
                import
    )
    
  }
  
  if(print) writeLines(ret)
  
  invisible(ret)

}
