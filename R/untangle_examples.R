#' @title Convert examples blocks in roxygen2 header to script
#' @description Converts and aggregates roxygen2 examples into a
#' single output file.
#' @param input character, file or directory
#' @param output character, file path of output, Default: './roxy_ex_to_file.R'
#' @return writes R file to disk
#' @details If output is set to NULL then output returned as invisible character object.
#' @export 
#' @rdname untangle_examples
#' @concept untangle
#' @author Jonathan Sidi
untangle_examples <- function(input, output = './roxy_ex_to_file.R'){
  
  if (length(input) == 1L && file.info(input)$isdir) {
    files <- list.files(path = input, pattern = ".+\\.[rR]$", full.names = TRUE)
  } else {
    files <- input
  }
  
  td <- file.path(tempdir(), "_sinew")
  
  if (!dir.exists(td)) {
    dir.create(td)
  }
  
  invisible(sapply(files,function(x) untangle(file = x, dir.out = td, keep.body = FALSE)))
  
  FILES <- list.files(td, full.names = TRUE)
  
  on.exit(expr = {
    unlink(td,recursive = TRUE,force = TRUE)
  },
  add = TRUE)
  
 x <- lapply(FILES,function(f){
    l <- readLines(f)
    oxy_current <- paste0(grep("^#'", l, value = TRUE), collapse = "\n")
    out <- get_oxy(oxy_current)['examples']  
    out <- gsub("#'",'',out)
    attr(out,'filename') <- gsub('\\.R$','',basename(f))
    out
  })
  
  x <- x[!sapply(x,is.na)]
  
  x <- lapply(x,function(y){
    
                  ex <- y[['examples']]
                  
                  for(idx in c('dontrun','dontshow','donttest'))
                      ex <- gsub(sprintf('\\\\%s',idx),
                                 sprintf('{#\\\\%s',idx),
                                 ex)
                  
                  if(length(ex)==0) return(NULL)
                  data.frame(file_name=attr(y,'filename'),
                             example=ex,
                             stringsAsFactors = FALSE)
                
        })
  
  x <- x[which(sapply(x,length)>0)]
  
  out <- paste0(sapply(x,function(y){
    
    pad_name <- sprintf('\n# %s ---- \n',y$file_name)
    
    sprintf('%s%s\n',pad_name,y$example)
    
  }),collapse = '\n')
  
  if(is.null(output)){
    return(invisible(out))
  }
  
  cat(out,
      file = output,
      sep='\n')
  
}
