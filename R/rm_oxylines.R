rm.oxylines <- function(this){
  idx <- grep("^\\s*#'",this$contents)
  if(length(idx)>0){
    seq.idx <- seqle(idx)
    if(length(seq.idx$lengths)>0){
      start.idx <- this$selection[[1]]$range[[1]][[1]]
      this.seqs <- seq.idx$lengths+seq.idx$values
      match.idx <- match(start.idx,this.seqs)
      if(!is.na(match.idx)){
        idx <- seq(seq.idx$values[match.idx],
                 length.out = seq.idx$lengths[match.idx])
      }else{
        idx <- NA
      }
    }
    
    if(all(!is.na(idx))){
      replace_location_starts <- mapply(rstudioapi::document_position, idx, 0, SIMPLIFY = FALSE)
      replacte_location_ends <- mapply(rstudioapi::document_position, idx + 1, 0, SIMPLIFY = FALSE)
      replace_range_list <- mapply(rstudioapi::document_range, replace_location_starts, replacte_location_ends, SIMPLIFY = FALSE)
      rstudioapi::modifyRange(location = replace_range_list, "",id = this$id)}
    }
  return(NULL)
}

seqle <- function(x,incr=1) { 
  if(!is.numeric(x)) x <- as.numeric(x) 
  n <- length(x)  
  y <- x[-1L] != x[-n] + incr 
  i <- c(which(y|is.na(y)),n) 
  list(lengths = diff(c(0L,i)),
       values = x[utils::head(c(0L,i)+1L,-1L)]) 
} 