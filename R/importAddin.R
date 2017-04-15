#' @importFrom rstudioapi getActiveDocumentContext
importAddin <- function() {
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()
  
  if(is.null(context$path)){
    f<-tempfile()
    cat(context$contents,file=f)
    context$path=f
  }
  
  makeImport(context$path,print = TRUE,format = 'oxygen')
  if(exists('f')) unlink(f)
}
