#' @importFrom utils help.search
make_seealso <- function(obj, cutOFF=3) {
  x <- make_import(obj, cut = cutOFF, print = FALSE)
  x <- grep("importFrom", strsplit(x, "#'")[[1]], value = TRUE)
  x <- sapply(x, function(y) {
    
    ret <- strsplit(gsub("\\n|@importFrom|^\\s+", "", y), " ")[[1]][-1]
    
    if(sinew_opts$get('markdown_links')){
      
      paste0(sprintf("[%s][%s::%s]", unique(ret[-1]), ret[1], unique(ret[-1])), collapse = ", ")
      
    }else{
      
      rdroot <- sapply(ret[-1],function(y){
        
        (utils::help.search(sprintf('^%s$',y),package = ret[1]))$matches$Name 
        
      },simplify = TRUE)
      
      paste0(sprintf("\\code{\\link[%s]{%s}}", ret[1], unique(rdroot)), collapse = ", ")  
      
    }
    
  }, 
  USE.NAMES = FALSE)
  
  xout <- paste0("#'  ", x)
  xout[1] <- sprintf("\n%s", xout[1])
  xout
}
