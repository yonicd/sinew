#' @title Populate import fields for documentation
#' @description Scrape `R` script to create import and importFrom calls for 
#' roxygen2, namespace or description files
#' @param script character, connection to pass to readLines, can be file path,
#'  directory path, url path
#' @param cut integer, number of functions to write as importFrom until 
#' switches to import, Default: NULL
#' @param print boolean, print output to console, Default: TRUE
#' @param format character, the output format must be in 
#' c('oxygen','description','import'), Default: 'oxygen'
#' @param desc_loc character, path to DESCRIPTION file, 
#' if not NULL then the Imports fields in the DESCRIPTION file, Default: NULL
#' @examples
#' 
#' # copy dummy package to tempdir
#' file.copy(system.file('pkg',package = 'sinew'),tempdir(),recursive = TRUE)
#' 
#' pkg_dir <- file.path(tempdir(),'pkg')
#' pkg_dir_R <- file.path(pkg_dir,'R')
#' pkg_dir_DESC <- file.path(pkg_dir,'DESCRIPTION')
#' 
#' # update namespaces in package functions
#' pretty_namespace(pkg_dir_R, overwrite = TRUE)
#' 
#' # update imports/importsFrom for roxygen2 tags
#' make_import(pkg_dir_R,format = 'oxygen')
#' 
#' # update Imports for DESCRIPTION file output to console
#' make_import(pkg_dir_R,format = 'description')
#'
#' # update Imports for DESCRIPTION file overwrite file
#' make_import(pkg_dir_R,format = 'description', desc_loc = pkg_dir)
#' 
#' cat(readLines(pkg_dir_DESC),sep = '\n')
#' 
#' # cleanup tempdir
#' unlink(pkg_dir, force = TRUE, recursive = TRUE)
#' 
#' @concept populate
#' @export
#' @importFrom utils capture.output getParseData
#' @importFrom tools file_ext
make_import <- function(script, cut = NULL, print = TRUE, format = "oxygen", desc_loc = NULL) {
  
  on.exit({  if (inherits(script, "function")) unlink(file) },add = TRUE)
  
  rInst <- paste0(.packages(all.available = TRUE), "::")

  if (inherits(script, "function")) {
    file <- tempfile()
    utils::capture.output(print(script), file = file)
    check.file <- readLines(file)
    cat(check.file[!grepl("^<", check.file)], file = file, sep = "\n")
  } else {
    if (all(nzchar(tools::file_ext(script)))) {
      file <- script
    } else {
      file <- list.files(script, full.names = TRUE, pattern = "\\.[R|r]$")
    }
  }

  pkg <- sapply(file, function(f) {
    parsed <- utils::getParseData(parse(f, keep.source = TRUE))
    parsed_f <- parsed[parsed$parent %in% parsed$parent[grepl("SYMBOL_PACKAGE", parsed$token)], ]
    parsed_f <- parsed_f[!grepl("::", parsed_f$text), c("parent", "token", "text")]

    PKGS <- unique(parsed_f$text[grepl("PACKAGE$", parsed_f$token)])

    ret <- sapply(PKGS, function(pkg) {
      x <- parsed_f[parsed_f$parent %in% parsed_f$parent[grepl(sprintf("^%s$", pkg), parsed_f$text)], ]
      fns <- unique(x$text[!grepl("SYMBOL_PACKAGE", x$token)])

      data.frame(pkg = pkg, fns = fns, stringsAsFactors = FALSE)
    }, simplify = FALSE)

    ret <- do.call("rbind", ret)
    rownames(ret) <- NULL

    if (format %in% c("oxygen", "import")) {
      ret <- sapply(unique(ret$pkg), function(pkg) {
        fns <- ret$fns[ret$pkg == pkg]
        if (format == "oxygen") {
          if (!is.null(cut) && length(fns) >= cut) {
            ret <- sprintf("#' @import %s", pkg)
          } else {
            ret <- sprintf("#' @importFrom %s %s", pkg, paste(fns, collapse = " "))
          }
        } else if (format == "import") {
          ret <- sprintf("import::from(%s, %s)", pkg, paste(fns, collapse = ", "))
        }
        ret
      })

      if (print) writeLines(paste(" ", f, paste(ret, collapse = "\n"), sep = "\n"))
    }

    return(ret)
  }, simplify = FALSE)

  if (format %in% c("oxygen", "import")) ret <- pkg

  if (format == "description") {
    ret <- do.call("rbind", pkg)
    ret <- paste(sort(unique(ret$pkg)), collapse = ",\n\t")

    if (print) writeLines(sprintf("Imports:\n\t%s", ret))

    if (!is.null(desc_loc)) {
      if (file.exists(file.path(desc_loc, "DESCRIPTION"))) {
        desc <- read.dcf(file.path(desc_loc, "DESCRIPTION"))

        import_col <- grep("Imports", colnames(desc))

        if (length(import_col) == 0) {
          desc <- cbind(desc, gsub("Imports: ", "\n\t", ret))

          colnames(desc)[ncol(desc)] <- "Imports"
        } else {
          desc[, "Imports"] <- gsub("Imports: ", "\n\t", ret)
        }

        write.dcf(desc, file = file.path(desc_loc, "DESCRIPTION"))
      }
    }
  }

  invisible(sapply(ret, paste0, collapse = "\n"))
}

#' @title Update Package Description File
#' @description Update package DESCRIPTION file Imports field
#' @param path character, path to R folder containing package functions
#' @param overwrite logical, overwrite the file, Default: TRUE
#' @return NULL
#' @details If overwrite is FALSE then the output will be returned to the console.
#' @examples 
#' 
#' # copy dummy package to tempdir
#' file.copy(system.file('pkg',package = 'sinew'),tempdir(),recursive = TRUE)
#' 
#' pkg_dir <- file.path(tempdir(),'pkg')
#' pkg_dir_R <- file.path(pkg_dir,'R')
#' pkg_dir_DESC <- file.path(pkg_dir,'DESCRIPTION')
#' 
#' # update namespaces in package functions
#' pretty_namespace(pkg_dir_R,overwrite = TRUE)
#' 
#' # send result to the console
#' update_desc(pkg_dir_R,overwrite = FALSE)
#' 
#' # overwrite the Imports field
#' update_desc(pkg_dir_R,overwrite = TRUE)
#' 
#' # view DESCRIPTION file
#' cat(readLines(pkg_dir_DESC),sep='\n')
#' 
#' # cleanup tempdir
#' unlink(pkg_dir,recursive = TRUE,force = TRUE)
#' 
#' @author Jonathan Sidi
#' @concept populate
#' @export
update_desc <- function(path, overwrite = TRUE){
  
  if(overwrite){
    desc_loc <- dirname(path)
  }else{
    desc_loc <- NULL
  }
  
  make_import(path, print = !overwrite, format = 'description', desc_loc = desc_loc)
  
}
