#' @param script character connection to pass to readLines, can be file path, directory path, url path
#' @param cut integer number of functions to write as importFrom until switches to import, Default: NULL
#' @param print boolean print output to console, Default: TRUE
#' @param format character the output format must be in c('oxygen','namespace','description'), Default: 'oxygen'
#' @param obj function or name of function
#' @param add_default logical to add defaults values to the end of the PARAM fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen fields, Default: NULL
#' @param path character or character vector of paths to files to search
#' @param cutoff numeric, minimal number of intersecting parameters to return, Default: 2
#' @param save_path boolean that allows for function to write template to man-roxygen subdirectory, Default: FALSE
#' @param df data.frame to convert to table
#' @param header boolean to control if header is created from names(df), Default: TRUE