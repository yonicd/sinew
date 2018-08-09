enframe_list <- function(x){
  do.call('rbind',lapply(names(x),function(y) data.frame(force_ns = y, text = x[[y]],stringsAsFactors = FALSE)))
}

summary_print <- function(obj,file){
  
  cat(
    sprintf("\nfunctions changed in '%s':\n%s\n\n",
            file,
            paste0(
              sprintf('  %s (line: %s cols: [%s-%s])',
                      numpad(obj$new_text),
                      numpad(obj$line1),
                      numpad(obj$col1),
                      numpad(obj$col2)),
              collapse = '\n')
            )
      )
}

numpad <- function(x){
  
  pad <- max(nchar(as.character(x)))
  
  if(inherits(x,c('numeric','integer')))
    ret <- sprintf(paste0('%0',pad,'d'),x) 
  
  if(inherits(x,'character'))
    ret <- sprintf('%s%s',x,strrep(' ',pad - nchar(x)))
    
  ret
}

