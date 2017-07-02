#' @title Make Oxygen Great Again
#' @description Update/append an R file that has roxygen2 headers already with updated information
#' @param path character path to R file
#' @param ... arguments to be passed to new makeOxygen
#' @param force.fields character, vector a field names that are in current header that are to be updated Default: NULL
#' @param dry.run boolean, write lines to console the output, Default: TRUE
#' @return character
#' @details Cross references fields already in the roxygen2 header and adds any new ones from the updated call. 
#' To force a change to a field add field name to force.fields.
#' @examples
#' \donttest{
#'  moga('https://raw.githubusercontent.com/metrumresearchgroup/ggedit/master/R/aesColour.R')
#' } 
#' @seealso 
#'  \code{\link[tools]{file_path_sans_ext}}
#' @rdname moga
#' @importFrom utils tail
#' @export
moga<-function(path, ... , force.fields=NULL, dry.run=TRUE){
  
  l <- readLines(path,warn = FALSE)
  fn_name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(path))
  assign(fn_name,eval(parse(text=l[!grepl("^#'",l)])))
  
  oxy_current <- paste0(grep("^#'",l,value=TRUE),collapse = '\n')
  
  oxy_new<-c()
  
  eval(parse(text=sprintf('oxy_new<-makeOxygen(%s,print=FALSE,...)',fn_name)))
  
  oxy_list <- sapply(c(current=oxy_current,new=oxy_new),function(this_oxy){
    this_oxy <- gsub('\\n$','',strsplit(this_oxy,"#' @")[[1]])
    this_oxy <- this_oxy[nchar(this_oxy)>0]
    this_oxy_vals <- gsub("^(.*?)\\s+",'',this_oxy)
    
    names(this_oxy_vals)<-regmatches(this_oxy, regexpr('(?:\\S+\\s+){0}(\\S+)', this_oxy))
    
    long_names<-which(names(this_oxy_vals)%in%c('importFrom','param'))
    if(length(long_names)>0){
      names(this_oxy_vals)[long_names]<-regmatches(this_oxy[long_names], regexpr('(?:\\S+\\s+){1}(\\S+)', this_oxy[long_names]))
      this_oxy_vals[long_names]<-sapply(strsplit(this_oxy[long_names],'\\s+'),
                                        function(x) paste(utils::tail(x,-2),collapse = ' '),
                                        USE.NAMES = FALSE)
    }

    this_oxy_vals
  })
  
  if(!is.null(force.fields)){
    oxy_current_names <- names(oxy_list[[1]])
    for(f in force.fields){
      if(f%in%oxy_current_names) oxy_list[[1]][f] <- oxy_list[[2]][f]
    }
  } 
  
  oxy_update <- c(oxy_list[[1]],oxy_list[[2]][setdiff(names(oxy_list[[2]]),names(oxy_list[[1]]))])
  
  oxy_update<-gsub("^#'" , "\n#'" , oxy_update)
  
  oxy_out <- sprintf("#' @%s %s",names(oxy_update),oxy_update)
  
  if(dry.run) writeLines(oxy_out)
  
  invisible(oxy_out)
}
