#' @title Parse package R files to create dictionary of unique parameter definitions
#' @description Given list of R files function returns roxygen2 template consisting of intersecting
#' parameter definitions
#' @param path character or character vector of paths to files to parse
#' @param save_path boolean that allows for function to write template to man-roxygen subdirectory, Default: FALSE
#' @return character/character vector of intersecting parameters
#' @export
#' @concept utility
#' @examples
#' makeDictionary('R')
makeDictionary <- function(path, save_path=FALSE) {
  if (!is.character(path)) {
    cli_abort("{.arg path} must be a character vector of file or directory paths")
  }
  
  if (is.null(names(path))) names(path) <- sprintf("Dictionary-%s", seq_along(path))

  ret <- sapply(names(path), function(p) {
    
    if (dir.exists(path[[p]])) {
      l <- list.files(path[[p]], pattern = "\\.[r|R]$", full.names = TRUE)
    } else if (file.exists(path[[p]])) {
      l <- path[[p]]
    }
    
    ret <- sapply(l, function(x) {
      grep("^#' @param", readLines(x, warn = FALSE), value = TRUE)
    })

    ret <- sort(unique(unlist(ret)))

    if (save_path) {
      dl <- unique(dirname(normalizePath(l)))
      save_dir <- gsub("/R", "/man-roxygen", dl)
      if (!dir.exists(save_dir)) dir.create(save_dir)
      save_name <- paste0(p, ".R")
      cat(ret, file = file.path(save_dir, save_name), sep = "\n")
    }
    ret
  })
  
  invisible(ret)
}

#' @rdname makeDictionary
#' @name make_dictionary
#' @export
make_dictionary <- makeDictionary
