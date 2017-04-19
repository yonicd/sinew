#' @param ... arguments to be passed to makeImport
#' @param ... arguments to pass to format
#' @param add_default boolean to add defaults values to the end of the PARAM fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen fields, Default: NULL
#' @param cut integer number of functions to write as importFrom until switches to import, Default: NULL
#' @param df data.frame to convert to table
#' @param dictionary character, path_to_dictionary, Default: 'roxygen-man/Dictionary.R'
#' @param format character the output format must be in c('oxygen','namespace','description'), Default: 'oxygen'
#' @param header boolean to control if header is created from names(df), Default: TRUE
#' @param obj function or name of function
#' @param path character or character vector of paths to files to parse
#' @param print boolean print output to console, Default: TRUE
#' @param print boolean write output to console, Default: TRUE
#' @param save_path boolean that allows for function to write template to man-roxygen subdirectory, Default: FALSE
#' @param script character connection to pass to readLines, can be file path, directory path, url path
