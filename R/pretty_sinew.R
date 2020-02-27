#' @title Convert File to R directory with pretty and oxygen
#' @description One function to run [pretty_namespace][sinew::pretty_namespace], [untangle][sinew::untangle] and [makeOxyFile][sinew::makeOxyFile]
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param dir.out character, path to save new R files, Default: NULL
#' @param keep.body boolean, if TRUE all non-functions will be saved to body.R in the parent 
#' directory of dir.out, Default: TRUE
#' @return Nothing, side effects is to create files
#' @details If dir.out is set to NULL all outputs are redirected into file.path(tempdir(),'sinew')
#' @concept namespace
#' @rdname pretty_sinew
#' @export 
#' @author Jonathan Sidi
pretty_sinew <- function(con = NULL,text = NULL,dir.out = NULL, keep.body = TRUE){

  if(!is.null(con)){
    text <- readLines(con)
  }
  
  temp_con <- tempfile(fileext = '.R')
  
  cat(text,file=temp_con,sep='\n')
  
  if(is.null(dir.out)){
      dir.out <- file.path(tempdir(),'sinew')
      dir.create(dir.out,showWarnings = FALSE)
  }
  
  pretty_namespace(con = temp_con,overwrite = TRUE)
  untangle(file = temp_con,dir.out = dir.out,keep.body = keep.body)
  makeOxyFile(dir.out,overwrite = TRUE)
}
