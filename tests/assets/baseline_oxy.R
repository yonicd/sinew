#some comment
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION, Default: 4
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname yy
#' @export 
#' @author Jonathan Sidi
 yy <- function(a=4){
   head(runif(10),a)
   # a comment

   myfun(x,y)

  sum(c(3,3))
 }
 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param v PARAM_DESCRIPTION, Default: 10
#' @param a PARAM_DESCRIPTION, Default: 8
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname zz
#' @export 
#' @author Jonathan Sidi
 zz <- function(v=10,a=8){
   head(runif(v),a)
 }
