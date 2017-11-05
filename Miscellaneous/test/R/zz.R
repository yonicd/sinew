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
#' @seealso 
#'  \code{\link[utils]{head}}
#'  \code{\link[stats]{runif}}
#' @rdname zz
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils head
#' @importFrom stats runif
zz <- function(v=10,a=8){
  utils::head(stats::runif(v),a)
}
