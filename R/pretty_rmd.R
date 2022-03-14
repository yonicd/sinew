#' @title Attach namespacing to Rmarkdown chunks
#' @description Apply pretty_namespace to Rmarkdown document
#' @param input character, path to input Rmd file 
#' @param output character, path to output Rmd file, Default: NULL
#' @param open_output boolean, open the output on.exit, Default: TRUE
#' @param create_library boolean, create library chunk, Default: TRUE
#' @param chunks numeric, indicies of chunks to run on, Default: NULL
#' @param \dots arguments to pass to pretty_namespace
#' @return character
#' @details 
#' 
#' If output is NULL then the returned lines are printed to console. 
#' If chunks is NULL then all the chunks are used.
#' 
#' @examples 
#' \dontrun{
#'   if(interactive()){
#'     pretty_rmd(input = system.file('example.Rmd',package = 'sinew'))
#'   }
#' }
#' @seealso [pretty_namespace][sinew::pretty_namespace]
#' @rdname pretty_rmd
#' @concept namespace
#' @export 
#' @importFrom rstudioapi navigateToFile
#' @importFrom utils select.list
#' @author Jonathan Sidi
pretty_rmd <- function(input, 
                       output = tempfile(fileext = '.Rmd'), 
                       open_output = TRUE, 
                       create_library = TRUE,
                       chunks = NULL,
                       ...){
  
  xo        <- readLines(input,warn = FALSE)
  
  x        <- rm_lib_chunk(xo)
  
  idx      <- list_chunks(x)
  
  if (is.null(idx)) {
    warning("No chunks found - returning as-is.")
    invisible(xo)
  }
  
  askenv   <- new.env()
  
  pretty_f <- pretty_rmd_inline
  
  if(create_library){
    
    pretty_f <- pretty_rmd_library

  }
  
  x <- pretty_f(x, idx, askenv, input, chunks,...)

  if(is.null(output)){
    cat(x, sep = '\n')
  }else{
    
    cat(x,file=output, sep = '\n')
    
    if(open_output)
      rstudioapi::navigateToFile(output)    
  }

  invisible(x)
}

#' @importFrom utils strcapture
list_chunks <- function(x){
  FROM <- grep('^```\\{(.*?)r',x)
  if (length(FROM) == 0) return(NULL)
  TO <- grep('^```$|^```\\s{1,}$',x)-1
  FROM <- FROM + 1
  ZERO <- FROM > TO
  if (any(ZERO)) {
    FROM <- FROM[!ZERO]
    TO <- TO[!ZERO]
  } 
  NAMES <- utils::strcapture("(?<=r\\s)(?:\\'|\\\")?([[:alnum:]\\s\\_\\.]+)",x[FROM - 1], data.frame(chunk_title = character()), perl = TRUE)
  NAMES$chunk <- sprintf("%03d", 1:NROW(NAMES))
  NAMES$lines <- paste0("lines", FROM,"-",TO)
  setNames(mapply(seq,from=FROM,to=TO, SIMPLIFY = FALSE), with(NAMES, mapply(function(.x, .y, .z) {
    out <- paste0("chunk", .x)
    if (!is.na(.y)) out <- paste0(out," - ", .y)
    if (!is.na(.z)) out <- paste0(out," - ",.z)
    return(out)
  }, chunk, chunk_title, lines)))
}

rm_lib_chunk <- function(x){
  
  this <- grep('^```(.*?)sinew libraries',x)
  
  if(length(this)>0){
    others <- grep('^```',x)
    x <- x[-c((this-1):others[which(this%in%others)+1])]    
  }
  
  x
}

pretty_rmd_library <- function(x, idx, askenv, input, chunks,...){
  
  userlibs <- gsub('library\\(|\\)','',grep('library\\((.*?)\\)',x,value = TRUE))
  
  pp <- sinew_opts$get('pretty_print')
  
  sinew_opts$set(pretty_print = FALSE)
  
  y <- lapply(idx,function(y,libs,askenv){
    tf <- tempfile(fileext = '.R')
    on.exit({unlink(tf)},add = TRUE)
    cat(x[y],file = tf,sep = '\n')
    
    x <- pretty_namespace(con = tf,..., askenv = askenv)
    
    x
  },
  libs   = userlibs,
  askenv = askenv
  )
  
  sinew_opts$set(pretty_print = pp)
  
  invisible(sapply(seq_along(y),function(x,input) if(length(y[[x]]$new_text)>0){
    pretty_print(
      y[[x]],
      file = input,
      chunk = sprintf('chunk %02d',as.numeric(x))
    )
  },input=input))
  
  y_text <- sapply(y,function(x){
    if(length(x$new_text)>0){
      x$namespace[!(x$namespace%in%c(NA,'base'))]
    } 
  })
  
  pkgs <- unique(unlist(y_text))
  pkgs <- setdiff(pkgs,userlibs)
  
  pkgs <- utils::select.list(
    choices = pkgs,
    multiple = TRUE,
    title = 'These unspecified namespaces were found in the document, see above output to locate relevant chunks\n choose the libraries to add to the top of the document')
  
  if(length(pkgs)>0){
    
    libs <- paste0(sprintf('library(%s)',pkgs),collapse = '\n')
    
    x[idx[[1]][1]-1] <- sprintf('```{r sinew libraries}\n%s\n```\n\n%s',libs,x[idx[[1]][1]-1])
  }
  x
}

pretty_rmd_inline <- function(x, idx, askenv,input, chunks,...){
  
  if(!is.null(chunks)){
    idx <- idx[chunks]
  }
  
  y <- mapply(function(y, nm){
    tf <- tempfile(paste0(nm,"_tmp_"), fileext = '.R')
    on.exit({unlink(tf)},add = TRUE)
    cat(x[y],file = tf,sep = '\n')
    pretty_namespace(con = tf,...,overwrite = TRUE, askenv = askenv)
    readLines(tf,warn = FALSE)
  }, idx, names(idx), SIMPLIFY = FALSE)
  
  for(i in seq_along(idx)){
    x[idx[[i]]] <- y[[i]]
  }
  
  x
}