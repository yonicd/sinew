#' @title Return roxygen2 parameter calls from parameter dictionary
#' @description Return roxygen2 parameter calls from the intersection of the parameters listed in
#'  the package dictionary and the formals of a function
#' @param obj function or name of function
#' @param dictionary character, path_to_dictionary, Default: 'roxygen-man/Dictionary-1.R'
#' @param print boolean print output to console, Default: TRUE
#' @return character vector
#' @examples 
#' repo='https://raw.githubusercontent.com/metrumresearchgroup/sinew/master/'
#' dict_loc=file.path(repo,'man-roxygen/Dictionary-1.R')
#' ls_param(sinew::makeOxygen,dictionary=dict_loc)
#' @export 
ls_param=function(obj,dictionary='man-roxygen/Dictionary-1.R',print=TRUE){
  dictionary_lines=readLines(dictionary,warn = FALSE)
  dictionary_lines=grep("#' @param ",dictionary_lines,value=TRUE)
  dictionary_params=sapply(strsplit(gsub("#' @param ",'',dictionary_lines),' '),'[',1)
  names(dictionary_lines)=dictionary_params
  if(is.character(obj)) obj=eval(parse(text=obj))
  nm=names(formals(obj))
  out=dictionary_lines[intersect(nm,dictionary_params)]
  if(print) cat(out,sep='\n')
  out=mapply(function(nm,out) gsub(sprintf("^#' @param %s\\s+|, Default:.*$",nm),'',out),
             nm=names(out),out=out)
  #out=gsub("^#' @param(.*?),|, Default:.*$",'',out)
  invisible(out)
}
