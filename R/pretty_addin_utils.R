get_range_rows <- function(rng){
  
  c(as.numeric(rng[[1]])[1],
    as.numeric(rng[[2]])[1])
}

get_range_cols <- function(rng){
  
  c(as.numeric(rng[[1]])[2],
    as.numeric(rng[[2]])[2])
}

range_null <- function(rng){
  (diff(get_range_rows(rng))==0)&(diff(get_range_cols(rng))==0)
}

#' @importFrom rstudioapi document_range
doc_range <- function(adc){
  
  contents <- adc$contents
  nc <- length(contents)
  
  rstudioapi::document_range(
    start = c(1,1),
    end   = c(nc,Inf)
  )
}

range_map <- function(rng){
  rows <- get_range_rows(rng)
  cols <- get_range_cols(rng)
  Map(c,
      Map(c, rows[1]:rows[2], cols[1]),
      Map(c, rows[1]:rows[2], cols[2])
  )
}

#' @importFrom rstudioapi insertText
pad_doc <- function(adc,times){
  if( times > 0 ){
    pos <- Map(c, (length(adc$contents)+1):(length(adc$contents)+times), 1)
    rstudioapi::insertText(pos, rep('\n',times),id = adc$id)
  }
  
}

pad_width <- function(tf,adc){
  length(readLines(tf))-length(adc$contents)
}

is_rmd <- function(adc){
  any(grepl('^```\\{(.*?)r',adc$contents))
}

find_chunks <- function(adc){
  lc <- list_chunks(adc$contents)
  ld <- do.call('rbind',lapply(seq_along(lc),function(x) data.frame(id=x,idx = lc[[x]])))
  rng <- get_range_rows(adc$selection[[1]]$range)[1]:get_range_rows(adc$selection[[1]]$range)[2]
  ret <- unique(ld$id[which(ld$idx%in%rng)])
  if(length(ret)==0){
    ret <- NULL
  }
  
  return(ret)
}
