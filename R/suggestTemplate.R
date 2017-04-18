#' @title Parse package R files to suggest Roxygen template of intersecting parameters
#' @description Given list of R files function returns Roxygen template consisting of intersecting
#' parameter definitions
#' @param path character or character vector of paths to files to search
#' @param cutoff numeric, minimal number of intersecting parameters to return, Default: 2
#' @param save_path boolean that allows for function to write template to man-roxygen subdirectory, Default: FALSE
#' @return character/character vector of intersecting parameters
#' @export 
#' @examples
#' suggestTemplate('R',cutoff = 3)
suggestTemplate=function(path,cutoff=2,save_path=FALSE){
  
  if(is.null(names(path))) names(path)=sprintf('template-%s',1:length(path))

  ret=sapply(names(path),function(p){
  l=list.files(path[[p]],full.names = TRUE)  
  lt=sapply(l,function(x){
    grep("#' @param",readLines(x,warn = FALSE),value=TRUE)
  })
  ltc=table(do.call('c',lt))
  ret=ltc[ltc>=cutoff]
  if(save_path){
    dl=unique(dirname(l))
    save_dir=gsub('/R','/man-roxygen',dl)
    save_name=paste0(p,'.R')
    cat(names(ret),file = file.path(save_dir,save_name),sep = '\n') 
  }
  ret
  })
  invisible(row.names(ret))
}
