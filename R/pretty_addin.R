#' @title Interactively run pretty functions in R files
#' @description Addin that scans the file source contents and attaches namespace
#' information.
#' @details
#' 
#' Either saved or untitled R files in the source editor may be used.
#' 
#' Highlight specific text, or not highlight at all and the whole document will be used.
#' 
#' @return NULL
#' @rdname pretty_addin
#' @importFrom rstudioapi getSourceEditorContext sendToConsole
#' @export
pretty_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  on.exit(rstudioapi::sendToConsole(''),add = TRUE)
  
  if(is_rmd(adc)){
    pretty_addin_rmd(adc)  
  }else{
    pretty_addin_r(adc) 
  }
  
}

#' @importFrom rstudioapi modifyRange
pretty_addin_r <- function(adc){
  
  file_path <- adc$path
  rng       <- adc$selection[[1]]$range
  txt       <- adc$selection[[1]]$text
  
  if(range_null(rng)){
    
    rng <- doc_range(adc)
    txt <- adc$contents
    
  }
  
  tf <- tempfile(fileext = '.r')
  cat(txt,file=tf,sep='\n')
  pretty_namespace(tf,ask = TRUE,overwrite = TRUE)
  
  rstudioapi::modifyRange(
    location = range_map(rng),
    text = readLines(tf), 
    id = adc$id
  )
  
}

#' @importFrom rstudioapi modifyRange
pretty_addin_rmd <- function(adc){
  
  file_path <- adc$path
  rng       <- adc$selection[[1]]$range
  txt       <- adc$selection[[1]]$text
  saved     <- nzchar(adc$path)
  rng       <- doc_range(adc)
  txt       <- adc$contents
  tf        <- tempfile(fileext = '.rmd')
  output    <- ifelse(saved,file_path,tf)
  
  cat(txt,file = tf,sep='\n')
  
  
  chunks    <- find_chunks(adc)
  libraries <- is.null(chunks)
  
  pretty_rmd(
    input = tf,
    output = output, 
    open_output = saved,
    create_library = libraries,
    chunks = chunks
    )
    
  if((!saved)&libraries){
      pad_doc(adc,pad_width(tf,adc))
      rng[[2]][[1]] <- rng[[2]][[1]] + pad_width(tf,adc)
  }
    
  rstudioapi::modifyRange(
    location = range_map(rng),
    text = readLines(tf), 
    id = adc$id
  )
  
}
