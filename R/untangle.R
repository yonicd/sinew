#' @title Split an R script by functions
#' @description Split a R script with multiple functions into multiple single function R files.
#' @param file character, path to R file, Default: ''
#' @param text character, vector of R commands, Default: NULL
#' @param dir.out character, path to save new R files, Default: NULL
#' @param keep.body boolean, if TRUE all non-funcitons will be saved to body.R in the parent 
#' directory of dir.out, Default: TRUE
#' @details body.R is written to the working directory and not dir.out. Functions that
#'  are objects in lists are treated as objects and will stay in body.R .
#' @return list of seperate functions
#' @examples
#' \dontrun{
#' txt <- "#some comment
#' #' @import utils
#' yy <- function(a=4){
#'  utils::head(runif(10),a)
#'  # a comment
#' }
#'
#' v <- 20
#'
#' #another comment
#' #' @import utils
#' zz <- function(v=10,a=3){
#'  utils::head(runif(v),pmin(a,v))
#' }
#'
#' zz(v)
#'
#' "
#' untangle(txt,dir.out = 'test')
#'
#' }
#' @rdname untangle
#' @export
#' @author Jonathan Sidi
#' @importFrom utils getParseData
untangle <- function(file = "", text = NULL, dir.out = NULL, keep.body = TRUE) {
  if (!is.null(text) & length(text) == 1) {
    text <- strsplit(text, "\n")[[1]]
  }
  if (nzchar(file)) {
    text <- readLines(file, warn = FALSE)
  }
  
  if (is.null(text)||length(text)==0) 
    return(NULL)
  
  p <- parse(text = text)
  p1 <- utils::getParseData(p)
  p1.filter <- p1$parent[with(p1, text == "function" & terminal == TRUE)]
  p2 <- p1[p1$id %in% (p1.filter + 1), ]
  p2$root <- as.numeric(rownames(p2))
  p.split <- sapply(p2$root, function(x, lines) {
    y <- p2[x == p2$root, c("line1", "line2")]
    y <- seq(y[, 1], y[, 2])
    y1 <- c(p1$line1[p1$parent %in% (-x)], y)
    lout <- lines[y1]
    fn.name <- p1$text[which(p1$id == x) + 1]
    
    if (!nzchar(fn.name)) #patch to find functions that are nested in lists that need to stay in the body
      return(list(name = fn.name, text = NULL))
    
    if (!is.null(dir.out)) {
      file.name <- sprintf("%s.R", gsub("[.]", "_", fn.name))
      cat(lout, file = file.path(dir.out, file.name), sep = "\n")
    }
    return(list(name = fn.name, text = y1))
  }, lines = text, simplify = FALSE)

  ret <- sapply(p.split, function(x) text[x$text])
  names(ret) <- sapply(p.split, function(x) x$name)

  if (keep.body) {
    check.body <- unlist(lapply(p.split, "[", 2))
    if (!is.null(check.body)) {
      body.text <- text[-check.body]
      rm.empty <- grep("^$", body.text)
      if (length(rm.empty) > 0) {
        body.text <- body.text[-rm.empty[diff(rm.empty) == 1]]
      }
      if (length(body.text) > 0) {
        if (!is.null(dir.out)){
          cat(body.text, file = file.path(dirname(dir.out), "body.R"), sep = "\n") 
        }
        ret$body <- body.text
      }
    }
  }

  invisible(ret)
}
