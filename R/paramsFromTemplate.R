#' @title Create roxygen parameter calls from subset of template
#' @description Create roxygen parameter calls from the intersection of the parameters listed in a template file
#' and the formals of a function
#' @param obj function or name of a function
#' @param template character, path_to_template, Default: 'roxygen-man/template.R'
#' @return character vector
#' @examples 
#' paramsFromTemplate(sinew::makeOxygen)
#' @export 
paramsFromTemplate=function(obj,template='roxygen-man/template.R'){
  template_lines=readLines(template,warn = FALSE)
  template_lines=grep("#' @param ",template_lines,value=TRUE)
  template_params=sapply(strsplit(gsub("#' @param ",'',template_lines),' '),'[',1)
  names(template_lines)=template_params
  if(is.character(obj)) obj=eval(parse(text=obj))
  nm=names(formals(obj))
  out=template_lines[intersect(nm,template_params)]
  names(out)<-NULL
  invisible(out)
  cat(out,sep='\n')
}
