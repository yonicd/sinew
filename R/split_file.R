#' @title Split an R script by functions
#' @description Split a R script with multiple functions into multiple single function R files.
#' @param text character, vector of R commands, Default: NULL
#' @param file character, path to R file, Default: ''
#' @param dir.out character, path to save new R files, Default: NULL
#' @return list of seperate functions
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname split_file
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils getParseData
split_file <- function(text=NULL,file= "",dir.out=NULL){
if(nzchar(file))
  text <- readLines(file,warn = FALSE)
if(is.null(text)) return(NULL)
p <- parse(text = text)
p1 <- utils::getParseData(p)
p2 <- p1[p1$parent==0,c('line1','line2')]  
p2$root <- as.numeric(rownames(p2))
p.split <- sapply(p2$root,function(x,lines) {
  y <- p2[x==p2$root,c(1,2)]
  y <- seq(y[,1],y[,2])
  y1 <- c(p1$line1[p1$parent%in%(-x)],y)
  lout <- lines[y1]
  if(!is.null(dir.out))
    if(dir.out==tempdir()){
      file.name <- sprintf('temp_fun%s.R',which(x==p2$root))
    }else{
      file.name <- sprintf('%s.R',p1$text[which(p1$id==x)+1])
    }
    cat(lout,file=file.path(dir.out,file.name),sep = '\n')
  return(lout)
  },lines=text)

invisible(p.split)
}
