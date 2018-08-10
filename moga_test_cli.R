#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param v PARAM_DESCRIPTION, Default: 10
#' @return my description
#' @author Jonathan Sidi
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @rdname zz
#' @source \url{http://somewhere.important.com/}
#' @import utils
zz <- function(v=10,a=8){
  utils::head(runif(v),a)
}

