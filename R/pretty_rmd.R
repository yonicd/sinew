#' @title Attach namespacing to Rmarkdown chunks
#' @description Apply pretty_namespace to Rmarkdown document
#' @param input character, path to input Rmd file 
#' @param output character, path to output Rmd file, Default: NULL
#' @param open_output boolean, open the output on.exit, Default: TRUE
#' @param create_library boolean, create library chunk, Default: TRUE
#' @param \dots arguments to pass to pretty_namespace
#' @return character
#' @details if output is NULL then the returned lines are printed to console. 
#' @examples 
#' if(interactive()){
#' rstudioapi::navigateToFile(system.file('example.Rmd',package = 'sinew'))
#' pretty_rmd(input = system.file('example.Rmd',package = 'sinew'))
#' }
#' @seealso 
#'  \code{\link[sinew]{pretty_namespace}}
#' @rdname pretty_rmd
#' @export 
#' @importFrom rstudioapi navigateToFile
#' @author Jonathan Sidi
pretty_rmd <- function(input, output = tempfile(fileext = '.Rmd'), open_output = TRUE, create_library = TRUE, ...){
  
  x <- readLines(input)
  
  FROM <- grep('^```\\{(.*?)r',x)+1
  TO <- grep('^```$',x)-1
  
  idx <- mapply(seq,from=FROM,to=TO)
  
  if(create_library){
    
    userlibs <- gsub('library\\(|\\)','',grep('library\\((.*?)\\)',x,value = TRUE))
    
    pp <- sinew_opts$get('pretty_print')
    sinew_opts$set(pretty_print = FALSE)
    y <- lapply(idx,function(y,libs){
      tf <- tempfile(fileext = '.R')
      on.exit({unlink(tf)},add = TRUE)
      cat(x[y],file = tf,sep = '\n')
      
      # for(l in libs){
      #   suppressMessages(loadNamespace(l))
      # }
      
      x <- sinew::pretty_namespace(con = tf,...)
      x$namespace
    },libs=userlibs)
    sinew_opts$set(pretty_print = pp)
    
    pkgs <- unique(unlist(y))
    pkgs <- setdiff(pkgs,c('base',userlibs))
    
    libs <- paste0(sprintf('library(%s)',pkgs),collapse = '\n')
    x[FROM[1]-1] <- sprintf('```{r libraries}\n%s\n```\n\n%s',libs,x[FROM[1]-1])
    
  }else{
    y <- lapply(idx,function(y){
      tf <- tempfile(fileext = '.R')
      on.exit({unlink(tf)},add = TRUE)
      cat(x[y],file = tf,sep = '\n')
      sinew::pretty_namespace(con = tf,...,overwrite = TRUE)
      readLines(tf)
    })
    
    for(i in seq_along(FROM)){
      x[FROM[i]:TO[i]] <- y[[i]]
    }  
  }

  ret <- paste0(x,collapse = '\n')
  
  cat(ret,file=output)
  
  if(open_output)
    rstudioapi::navigateToFile(output)
  
  invisible(x)
}