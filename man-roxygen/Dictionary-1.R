#' @param .file, path to an .R file, character vector of length 1
#' @param \dots additional parameters passed to \code{makeOxygen}
#' @param \dots arguments to be passed to makeImport
#' @param \dots arguments to be passed to new makeOxygen
#' @param \dots arguments to pass to format
#' @param \dots arguments to pass to pretty_namespace
#' @param add_default boolean to add defaults values to the end of the PARAM 
#' @param add_fields character vector to add additional roxygen2 fields, 
#' @param ask boolean, If TRUE then a \code{\link[utils]{menu}} will be created for the use to
#' @param askenv environment, environment that stores names of functions to force in ask,
#' @param chunks numeric, indicies of chunks to run on, Default: NULL
#' @param con character, path to file or directory that contains script, Default: NULL
#' @param create_library boolean, create library chunk, Default: TRUE
#' @param cut integer, number of functions to write as importFrom until 
#' @param desc_loc character, path to DESCRIPTION file, 
#' @param df data.frame to convert to table
#' @param dictionary character, path_to_dictionary, Default: 'roxygen-man/Dictionary-1.R'
#' @param dir.body character, path to save body.R files, Default: dirname(dir.out)
#' @param dir.out character, path to save new R files, Default: NULL
#' @param dry.run boolean, write lines to console the output, Default: TRUE
#' @param file character, path to R file, Default: ''
#' @param force list, named list of functions to force over the
#' @param force.fields character, vector a field names that are in current header that are to be updated Default: NULL
#' @param format character, the output format must be in 
#' @param header boolean to control if header is created from names(df), Default: TRUE
#' @param ignore list, named list of functions to ignore (seee details), Default: NULL
#' @param input character, file or directory
#' @param input character, path to input Rmd file 
#' @param input character, vector of path(s) to one or more .R files, a path to directory containing .R files, Default: NULL
#' @param keep.body boolean, if TRUE all non-functions will be saved to body.R in the parent 
#' @param obj function or name of function
#' @param open_output boolean, open the output on.exit, Default: TRUE
#' @param output character, file path of output, Default: './roxy_ex_to_file.R'
#' @param output character, path to output Rmd file, Default: NULL
#' @param overwrite boolean, If TRUE overwrites file(s), FALSE writes "Oxy"- prefixed files in the same directory, Default: FALSE
#' @param overwrite boolean, overwrite contents of input file, Default: FALSE
#' @param overwrite boolean, overwrite original file, Default: FALSE
#' @param overwrite PARAM_DESCRIPTION, Default: TRUE
#' @param path character or character vector of paths to files to parse
#' @param path character path to R file
#' @param path PARAM_DESCRIPTION
#' @param print boolean print output to console, Default: TRUE
#' @param print boolean, print output to console, Default: TRUE
#' @param save_path boolean that allows for function to write template to man-roxygen subdirectory, Default: FALSE
#' @param script character, connection to pass to readLines, can be file path,
#' @param sos boolean, apply sos search for uninstalled libraries, Default: FALSE
#' @param text character, vector of R commands, Default: NULL
#' @param text character, vector that contains script, Default: NULL
#' @param use_dictionary character, path_to_dictionary, Default: NULL
#' @param verbose boolean, If TRUE will print output to console and open edited files in the editor viewer, Defulat: interactive()
