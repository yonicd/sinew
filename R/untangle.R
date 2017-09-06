#' @title Split an R script by functions
#' @description Split a R script with multiple functions into multiple single function R files.
#' @param text character, vector of R commands, Default: NULL
#' @param file character, path to R file, Default: ''
#' @param dir.out character, path to save new R files, Default: NULL
#' @param keep.body boolean, if TRUE all non-funcitons will be saved to body.R, Default: TRUE
#' @return list of seperate functions
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname untangle
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils getParseData
untangle <- function(text = NULL,file = "", dir.out = NULL, keep.body = TRUE){
if(nzchar(file))
  text <- readLines(file,warn = FALSE)
if(is.null(text)) return(NULL)
p <- parse(text = text)
p1 <- utils::getParseData(p)
p1.filter <- p1$parent[with(p1,text=='function'&terminal==TRUE)]
p2 <- p1[p1$id%in%(p1.filter+1),]
p2$root <- as.numeric(rownames(p2))
p.split <- sapply(p2$root,function(x,lines) {
  y <- p2[x==p2$root,c('line1','line2')]
  y <- seq(y[,1],y[,2])
  y1 <- c(p1$line1[p1$parent%in%(-x)],y)
  lout <- lines[y1]
  fn.name <- p1$text[which(p1$id==x)+1]
  if(!is.null(dir.out)){
    file.name <- sprintf('%s.R',fn.name)
    cat(lout,file=file.path(dir.out,file.name),sep = '\n')}
  return(list(name=fn.name,text=y1))
  },lines=text,simplify = FALSE)

ret <- sapply(p.split,function(x) text[x$text])
names(ret) <- sapply(p.split,function(x) x$name)

if( keep.body ){
  body.text <- text[-unlist(lapply(p.split,'[',2))]
  cat(body.text,file=file.path(dir.out,'body.R'),sep = '\n')
  ret$body <- body.text
}

invisible(ret)
}
