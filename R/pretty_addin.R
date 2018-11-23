#' @title Interactively run pretty functions
#' @description Addin that scans the file source contents and attaches namespace
#' information.
#' @details
#' 
#' Either saved or untitled R and Rmd files in the source editor may be used.
#' 
#' In R files you can highlight specific text, or not highlight at all and
#' the whole document will be used.
#' 
#' In Rmd files if you highlight text it must be within a single chunk, or not highlight at all and
#' the whole document will be used to create a new chunk at the top of the document with 
#' the libraries needed to run the document.
#'
#' 
#' @return NULL
#' @rdname pretty_addin
#' @importFrom rstudioapi getSourceEditorContext sendToConsole modifyRange
#' @export
pretty_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  on.exit(rstudioapi::sendToConsole(''),add = TRUE)
  
  file_path <- adc$path
  rng       <- adc$selection[[1]]$range
  txt       <- adc$selection[[1]]$text
  saved     <- nzchar(adc$path)
  file_type <- ifelse(is_rmd(adc),'rmd','r')
  file_type <- ifelse(range_null(rng),file_type,'r')
  
  if(range_null(rng)){
    
    rng <- doc_range(adc)
    txt <- adc$contents
    
  }else{
    
    if(( file_type == 'r' ) & ( grepl('```\\{(.*?)r',txt) ) ){
      message('Highlighting text in a Rmd file is limited to within a single chunk')
      message('To run on whole file do not highlight any text')
      return(invisible(NULL))
    }
    
  }
  
  tf <- tempfile(sprintf('.%s',file_type))
  cat(txt,file=tf,sep='\n')
  
  if(file_type=='rmd'){
    
    output <- ifelse(saved,file_path,tf)

    pretty_rmd(tf,output = output, open_output = saved)
    
    if(!saved){
      pad_doc(adc,pad_width(tf,adc))
      rng[[2]][[1]] <- rng[[2]][[1]] + pad_width(tf,adc)
    }else{
      return(invisible(NULL))  
    }
    
  }
  
  if(file_type%in%c('r')){
    pretty_namespace(tf,ask = TRUE,overwrite = TRUE)
  }
  
  rstudioapi::modifyRange(
    location = range_map(rng),
    text = readLines(tf), 
    id = adc$id
  )
  
}
