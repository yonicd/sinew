pretty_parse <- function(txt){
  
  p <- parse(text = txt)
  
  p1 <- utils::getParseData(p)
  
  rmParent <- p1$parent[p1$token == "SYMBOL_PACKAGE"]
  
  p1[p1$token == "SYMBOL_FUNCTION_CALL" & !p1$parent %in% rmParent, ]
  
}

#' @importFrom crayon red
#' @importFrom stringi stri_sub
pretty_shift <- function(txt, sym.funs, nm, overwrite, force, ignore){
  
  sym.funs <- pretty_manip(sym.funs, force, ignore)
  
  if(!overwrite){
    sym.funs$new_text <- crayon::red(sym.funs$new_text)
  }
  
  idx <- which(!sym.funs$namespace %in% c("base", NA))
  
  sym.funs.i <- split(sym.funs[idx,],sym.funs$line1[idx])
  
  sym.funs.i.shift <- lapply(sym.funs.i,function(sf){
    
    x <- rep(0,nrow(sf))
    
    if(nrow(sf)>1){
      for(i in 2:nrow(sf)){
        
        x[i:nrow(sf)] <- x[i] + (nchar(sf$new_text[i - 1]) - nchar(sf$text[i - 1]))
        
      }
    }
    
    sf$col_shift <- x
    
    sf
  })
  
  sym.funs.shift <- do.call('rbind',sym.funs.i.shift)
  
  sym.funs.shift$col1 <- sym.funs.shift$col1 + sym.funs.shift$col_shift
  sym.funs.shift$col2 <- sym.funs.shift$col2 + sym.funs.shift$col_shift
  
  for(i in 1:length(idx)){
    stringi::stri_sub(
      str = txt[sym.funs.shift$line1[i]],
      from = sym.funs.shift$col1[i],
      to = sym.funs.shift$col2[i]) <- sym.funs.shift$new_text[i]  
  }
  
  if (overwrite) {
    cat(txt, sep = "\n", file = nm)
    pretty_print(sym.funs,file = nm)
    
  } else {
    
    pretty_print(sym.funs,file = nm)
    writeLines(crayon::white(txt))
    
  }
  
  txt
}

pretty_manip <- function(sym.funs, force, ignore){
  
  sym.funs$action <- ''
  
  if(!is.null(force)){
    sym.funs <- pretty_merge(sym.funs,force,'replace')
  }
  
  if(!is.null(ignore)){
    sym.funs <- pretty_merge(sym.funs,ignore,'remove')
  }
  
  sym.funs$new_text <- sprintf('%s::%s',sym.funs$namespace, sym.funs$text)  
  
  sym.funs
}

#' @importFrom cli symbol
pretty_merge <- function(e1,e2,action = 'relpace'){

  e1 <- merge(e1,enframe_list(e2),by = 'text',all.x = TRUE)
  
  e1 <- switch(action,
         'replace'={
           e1$namespace[!is.na(e1$force_ns)] <- e1$force_ns[!is.na(e1$force_ns)]
           e1
         },
         'remove'={
           e1[is.na(e1$force_ns),]
         })

  e1$action[!is.na(e1$force_ns)] <- cli::symbol$checkbox_on
  
  e1$force_ns <- NULL
  
  e1[order(e1$id),]
}

#' @importFrom sos findFn
#' @importFrom utils help.search
pretty_find <- function(NMPATH, sos, sym.funs, funs){
  
  check_global <- ls(envir = get(search()[1]))
  
  if (length(check_global)>0){
    global.funs <- check_global[sapply(check_global, function(x) class(get(x)) == "function")]
    funs <- funs[!funs %in% global.funs]  
  }
  
  for (x in NMPATH) {
    if (length(funs) == 0) break
    
    found <- funs %in% mf(x, funs)
    
    sym.funs$namespace[sym.funs$text %in% funs[found]] <- x
    
    funs <- funs[!found]
  }
  
  if (length(funs) > 0) {
    for (fun in funs) {
      suppressWarnings(fun.help <- utils::help.search(sprintf("^%s$", fun), ignore.case = FALSE))
      if (nrow(fun.help$matches) > 0) {
        sym.funs$namespace[sym.funs$text %in% fun] <- fun.help$matches$Package[1]
        funs <- funs[-match(fun, funs)]
      }
    }
  }
  
  if (sos & length(funs) > 0) {
    for (fun in funs) {
      suppressWarnings(fun.sos <- sos::findFn(fun, maxPages = 1, verbose = 0))
      if (nrow(fun.sos)) {
        sym.funs$namespace[sym.funs$text %in% fun] <- fun.sos$Package[1]
        funs <- funs[-match(fun, funs)]
      }
    }
  }
  
  sym.funs
  
}

enframe_list <- function(x){
  do.call('rbind',lapply(names(x),function(y) data.frame(force_ns = y, text = x[[y]],stringsAsFactors = FALSE)))
}

#' @importFrom crayon red strip_style
#' @importFrom cli symbol
pretty_print <- function(obj,file){
  
  if(!sinew_opts$get('pretty_print'))
    return(NULL)
  
  if(nrow(obj)==0)
    return(NULL)
  
  if(!grepl('\\.[rR]$',file))
    file <- 'text object'
  
  obj <- obj[!obj$namespace %in% c("base"),]
    
  obj$new_text <- crayon::strip_style(obj$new_text)
  
  obj$symbol <- ifelse(is.na(obj$namespac),crayon::red(cli::symbol$cross),cli::symbol$tick)
  
  obj$new_text <- gsub('^NA::','',obj$new_text)

  tbl <- table(obj$new_text)

  counts <- setNames(as.numeric(tbl),names(tbl))
    
  obj <- obj[!duplicated(obj$new_text),]

  obj$counts <- NA
  
  obj$counts[match(obj$new_text,names(counts))] <- counts
  
  obj$out_text <- sprintf(' %s %s (%s) %s',
                          obj$symbol,
                          numpad(obj$new_text),
                          numpad(obj$counts),
                          obj$action
                          )
    
  cat(
    sprintf("\nfunctions changed in '%s':\n\n%s: found, %s: not found, (): instances, %s: user intervention\n\n%s\n\n",
            file,
            cli::symbol$tick,
            crayon::red(cli::symbol$cross),
            cli::symbol$checkbox_on,
            paste0(obj$out_text,collapse = '\n')
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

mf <- function(x, pat) {
  ns <- try(
    {
      
      if(!isNamespaceLoaded(x)){
        y <- attachNamespace(x)  
      }
      
      ls(
        envir = sprintf('pacakge:%s',x),
        pattern = sprintf("^(%s)$", paste0(pat, collapse = "|"))
        )
    },
    silent = TRUE
  )
  
  if (class(ns) == "try-error") {
    ns <- vector("character")
  }
  
  ns
}

