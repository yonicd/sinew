#' @title Split an R script by functions
#' @description Split a R script with multiple functions into multiple single function R files.
#' @param file character, path to R file, Default: ''
#' @param text character, vector of R commands, Default: NULL
#' @param dir.out character, path to save new R files, Default: NULL
#' @param keep.body boolean, if TRUE all non-functions will be saved to body.R in the parent 
#' directory of dir.out, Default: TRUE
#' @param dir.body character, path to save body.R files, Default: dirname(dir.out)
#' @details Functions that are objects in lists are treated as objects and will stay in body.R .
#' @return list of seperate functions
#' @examples
#' 
#' test_dir <- file.path(tempdir(),'sinew_test')
#' 
#' dir.create(test_dir)
#' 
#' txt <- "#some comment
#' yy <- function(a=4){
#'  head(runif(10),a)
#'  # a comment
#' }
#'
#' v <- 20
#'
#' #another comment
#' zz <- function(v=10,a=3){
#'  head(runif(v),pmin(a,v))
#' }
#'
#' zz(v)
#'
#' "
#' 
#' untangle(text = txt,dir.out = test_dir)
#' 
#' list.files(tempdir(), recursive = TRUE, pattern = '.R$')
#' 
#' cat( readLines(file.path(test_dir,'yy.R')), sep = '\n')
#' 
#' cat( readLines(file.path(test_dir,'zz.R')), sep = '\n')
#' 
#' cat( readLines(file.path(tempdir(),'body.R')), sep = '\n')
#' 
#' unlink(test_dir, force = TRUE, recursive = TRUE)
#'
#'
#' @rdname untangle
#' @export
#' @concept untangle
#' @author Jonathan Sidi
#' @importFrom utils getParseData
untangle <- function(file = "", text = NULL, dir.out = "", keep.body = TRUE, dir.body = dirname(dir.out)) {
  if (!is.null(text) & length(text) == 1) {
    text <- strsplit(text, "\n")[[1]]
  }
  if (nzchar(file)) {
    text <- readLines(file, warn = FALSE)
  }
  
  if (is.null(text)||length(text)==0) 
    return(NULL)
  
  p <- parse(text = text,keep.source = TRUE)
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
    
    # patch to find functions that are nested in lists that need to stay in the body
    if (!nzchar(fn.name)) 
      return(list(name = fn.name, text = NULL))

    return(list(name = fn.name, text = y1))
  }, lines = text, simplify = FALSE)

  # prune nested functions
  p.split.idx <- which.parents(lapply(p.split, function(x) x$text))
  p.split <- p.split[p.split.idx]

  ret <- lapply(p.split, function(x) text[x$text])
  names(ret) <- lapply(p.split, function(x) x$name)
  
  # writing to disk
  
  if (nzchar(dir.out)) {
    invisible({
      lapply(p.split,function(x, dir.out, text){
        file_name <- sprintf("%s.R", gsub("[.]", "_", x$name))
        file_path <- file.path(dir.out, file_name)
        cat(text[x$text], file = file_path, sep = "\n")
      },dir.out = dir.out, text = text)    
    })
    
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
            cat(body.text, file = file.path(dir.body, "body.R"), sep = "\n") 
          }
          ret$body <- body.text
        }
      }
    }
  }

  invisible(ret)
}

which.parents <- function(obj){
  which(obj %in% find.parents(obj))
}

find.parents <- function(obj){
  i <- 1
  flag <- TRUE
  while(flag & i <= length(obj)){
    rem_i <- vector(mode = 'numeric')
    for(ii in (i+1):length(obj)){
      if(length(setdiff(obj[[ii]],obj[[i]]))==0){
        rem_i <- c(rem_i,ii)
      }
    }
    flag <- length(rem_i)>0
    obj[rem_i] <- NULL
    i <- i + 1
  }
  obj
}
