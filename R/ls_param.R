#' @title Return roxygen parameter calls from parameter dictionary
#' @description Return roxygen parameter calls from the intersection of the parameters listed in
#'  the package dictionary and the formals of a function
#' @param obj function or name of function
#' @param dictionary character, path_to_dictionary, Default: 'roxygen-man/Dictionary-1.R'
#' @return character vector
#' @examples 
#' dict_loc='https://raw.githubusercontent.com/yonicd/sinew/master/man-roxygen/Dictionary-1.R'
#' ls_param(sinew::makeOxygen,dictionary=dict_loc)
#' @export 
ls_param=function(obj,dictionary='man-roxygen/Dictionary-1.R'){
  dictionary_lines=readLines(dictionary,warn = FALSE)
  dictionary_lines=grep("#' @param ",dictionary_lines,value=TRUE)
  dictionary_params=sapply(strsplit(gsub("#' @param ",'',dictionary_lines),' '),'[',1)
  names(dictionary_lines)=dictionary_params
  if(is.character(obj)) obj=eval(parse(text=obj))
  nm=names(formals(obj))
  out=dictionary_lines[intersect(nm,dictionary_params)]
  names(out)<-NULL
  invisible(out)
  cat(out,sep='\n')
}
