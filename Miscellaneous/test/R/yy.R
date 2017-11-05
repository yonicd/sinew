#some comment
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a something, Default: 4
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
#' @rdname yy
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils head
#' @importFrom stats runif
yy <- function(a=4){
  utils::head(stats::runif(10),a)
  # a comment
}
