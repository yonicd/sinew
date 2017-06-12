#' @importFrom rstudioapi getActiveDocumentContext insertText
oxygenAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  obj_name=gsub("[\"\']",'',context$selection[[1]]$text)
  assign(obj_name,eval(parse(text=obj_name)))
  eval(parse(text=sprintf('ret<-makeOxygen(%s,print=FALSE)',obj_name)))
  cl=NULL
  eval(parse(text=sprintf('cl<-class(%s)',obj_name)))
  if(cl=='function') ret=paste(ret,obj_name,sep='\n')
  rstudioapi::insertText(text = ret)
}
