#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a numeric, set the head to trim from random unif Default: 4
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \\dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \\code{\\link[utils]{head}}
#'  \\code{\\link[stats]{runif}}
#' @rdname yy
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils head
#' @importFrom stats runif
yy <- function(a=4,b=2){
  x <- utils::head(stats::runif(10*b),a)
  stats::quantile(x,probs=.95)
  # a comment
}
