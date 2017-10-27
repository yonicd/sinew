#' @title Create _sinewconfig.yml
#' @description Create _sinewconfig.yml file in project root directory
#' @return nothing
#' @examples 
#' \dontrun{
#' create_yml()
#' }
#' @rdname create_yml
#' @export 
#' @author Jonathan Sidi
#' @importFrom rstudioapi getActiveProject
create_yml <- function(){
  
  if(is.null(rstudioapi::getActiveProject()))
    stop('Not in RStudio Project')
  
  yml <- file.path(rstudioapi::getActiveProject(),'_sinewconfig.yml')
  ignore <- file.path(rstudioapi::getActiveProject(),'.Rbuildignore')
  no_ignore <- !file.exists(ignore)
  
  if( !file.exists(yml) ){
    file.create(yml)
    message(yml,' created')
  }
      
  
  
  if( no_ignore ){
    
    file.create(ignore)
    
    message(ignore,' created')
    
  }
  
  current_ignore <- readLines('.Rbuildignore')  
    
  if(!any(grepl('\\^\\.\\*\\\\\\.yml\\$',current_ignore))){
      cat("^.*\\.yml$",file=ignore,sep='\n',append = !no_ignore)  
    
      message("^.*\\.yml$ added to ", ignore)
    }
  
  
  
  
}