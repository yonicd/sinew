#' @title Inserts roxygen2 skeletons in file(s).
#' @description Applies \code{makeOxygen} function to all functions/dataframes in supplied file(s)
#' @param input character, vector of path(s) to one or more .R files, a path to directory containing .R files, Default: NULL
#' @param overwrite logical, If TRUE overwrites file(s), FALSE writes "Oxy"- prefixed files in the same directory, Default: FALSE
#' @param verbose logical, If TRUE will print output to console and open edited files in the editor viewer, Default: interactive()
#' @param print boolean print output from each processed file to console. Default: FALSE
#' @inheritParams makeOxygen
#' @param dir.out character, path to save new R files, Default: NULL
#' @inheritDotParams makeOxygen -obj -copy -title -description
#' @return Nothing. Writes files with roxygen2 comments as a side effect
#' @author Anton Grishin
#' @details If an object cannot be found it will be sourced into a temporary environment.
#' If the file already contains roxygen2 comments they will be deleted to avoid duplication.
#' Some functions may require attaching additional packages. For instance, if functions
#' were defined with purrr's \code{compose} or \code{partial} functions, omission of \code{purr::} in definitions will
#' require \code{library(purrr)} before proceeding with \code{makeOxyFile}.
#' @examples
#'  
#' # copy dummy package to tempdir
#'   file.copy(system.file('pkg',package = 'sinew'),tempdir(),recursive = TRUE)
#'   pkg_dir <- file.path(tempdir(),'pkg')
#'   pkg_dir_R <- file.path(pkg_dir,'R')
#' 
#' # update namespaces in package functions
#'   pretty_namespace(pkg_dir_R, overwrite = TRUE)
#'    
#'  # test on one R file 
#'  # this will create a new R file called 'oxy-yy.R' in the same directory
#'    makeOxyFile(file.path(pkg_dir_R,'yy.R'))
#'  
#'  # Remove the file
#'    unlink(file.path(pkg_dir_R,'oxy-yy.R'))
#'  
#'  # Test on all R files in directory and overwrite the contents
#'    makeOxyFile(pkg_dir_R, overwrite = TRUE)
#'  

#'  # Remove Skeleton
#'    rmOxygen(file.path(pkg_dir_R,'yy.R'))
#'    rmOxygen(file.path(pkg_dir_R,'zz.R'))
#'    
#'  # adds more fields to defaults, passes "cut" to make_import
#'    sinew_opts$append(list(add_fields=c("concept", "describeIn")))
#'    makeOxyFile(file.path(pkg_dir_R,'yy.R'), cut = 5)
#'  
#'  # cleanup 
#'    unlink(pkg_dir, recursive = TRUE, force = TRUE)
#'    sinew_opts$restore()
#'    
#' @export
#' @seealso [makeOxygen][sinew::makeOxygen]
#' @rdname makeOxyFile
#' @concept populate
#' @importFrom rstudioapi isAvailable navigateToFile
#' @importFrom cli cli_abort cli_alert_info cli_bullets cli_alert_warning
makeOxyFile <- function(input = NULL,
                        overwrite = FALSE,
                        verbose = interactive(),
                        print = FALSE,
                        markdown = FALSE,
                        dir.out = NULL,
                        ...) {
  if (is.null(input)) input <- file.choose()
  
  if (!is.character(input)) {
    cli_abort("{.arg input} must be a character vector, not {.obj_type_friendly {input}}")
  }
  
  if (!is.logical(overwrite)) {
    cli_abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {overwrite}}")
  }
  
  if (length(input) == 1L && dir.exists(input)) {
    files <- list.files(path = input, pattern = ".+\\.[rR]$", full.names = TRUE)
    files <- grep("/(?!Oxy)\\w+\\.[rR]$", files, perl = TRUE, value = TRUE)
  } else {
    files <- input
  }

  if (!all(grepl("\\.[rR]$", basename(files)))) {
    cli_abort("Supplied file(s) is not an .R file!")
  }

  neg_msg <- "No functions found in\n"
  
  files <- files[sapply(files,scan_for_content)]
  
  if(length(files)==0){
    return(invisible(NULL))
  }
  
  append_to_lines <- function(.id, .str) {
    unlist(append(
      .str, strsplit(
        oxy_lst[[which(ins_id == .id)]],
        split = "\n", fixed = TRUE
      ),
      after = .id
    ))
  }
  
  if('RcppExports.R'%in%basename(files)){
    rccp_ans <- utils::menu(
      choices = c('Yes','No'),
      title = 'RcppExports.R was found, do you want to run makeOxyFile on it?')
    
    if(rccp_ans==2){
      files <- files[-which('RcppExports.R'%in%basename(files))]
    }
  }
  
  for (FILE in files) {
    
    lines <- readLines(FILE, warn = FALSE)
    
    lines <- lines[!grepl("^\\s*#'", lines)]
    
    objs <- gsub(
      "\\s*([[:alnum:]._]+).*",
      "\\1", grep("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines, value = TRUE)
    )
    
    nenv <- new.env()
    
    if (!all(objs %in% ls(envir = parent.frame()))) {
      sys.source(FILE, nenv, keep.source = TRUE)
    }

    sel0 <- seq_along(objs)

    if ("nenv" %in% ls()) {
      sel0 <- objs %in% ls(envir = nenv)
      objs <- objs[sel0]
    }

    sel_obj <- vapply(
      objs, function(x) {
        inherits(get(x, envir = nenv), c("data.frame", "function"))
      },
      FUN.VALUE = logical(1)
    )

    if (!any(sel_obj)) warning(neg_msg, normalizePath(FILE))

    objs <- objs[sel_obj]

    oxy_lst <- lapply(objs, function(obj_name, thisenv, ...) {
      assign(obj_name, get(obj_name, envir = thisenv))
      eval(
        parse(
          text = sprintf(
            "makeOxygen(%s,..., title=NULL, description=NULL, markdown=%s, print=%s, copy=FALSE)",
            obj_name, markdown, print
          ),
          keep.source = TRUE))
    }, thisenv = nenv, ...)

    ins_id <- which(grepl("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines)) - 1L
    ins_id <- ins_id[sel0]
    ins_id <- ins_id[sel_obj]

    for (i in rev(ins_id)) {
      lines <- append_to_lines(i, lines)
    }
    
    if (is.null(dir.out)) {
      dir.out <- dirname(FILE)
    }
    
    new_name <- if (overwrite) {
      FILE
    } else {
      file.path(dir.out, paste("oxy", basename(FILE), sep = "-"))
    }
    writeLines(lines, new_name)
  }
  
  if (is.null(dir.out)) {
    dir.out <- dirname(files)
  } else if (overwrite) {
    cli::cli_alert_warning(
      "{.arg dir.out} is ignored when {.arg overwrite} is {.code TRUE}")
  }
  
  oxyfiles <- if (overwrite) {
    files
  } else {
    file.path(dir.out, paste("oxy", basename(files), sep = "-"))
  }
  
  if (length(input) > 0L) {
    if (verbose) {
      if (rstudioapi::isAvailable()) {
        for (i in oxyfiles) rstudioapi::navigateToFile(i)
      } else {
        file.show(oxyfiles)
      }

      cli_alert_info("File(s) with roxygen2 comment templates written to:")
      cli_bullets(
        normalizePath(oxyfiles, winslash = "/") 
      )
      
    }
  }
  
  invisible(oxyfiles)
}

#' @rdname makeOxyFile
#' @name make_oxy_file
#' @export
make_oxy_file <- makeOxyFile

#' @importFrom cli cli_warn
scan_for_content <- function(FILE, neg_msg = "No functions found in\n"){
  
  lines <- readLines(FILE, warn = FALSE)
  
  lines <- lines[!grepl("^\\s*#'", lines)]
  
  objs <- gsub(
    "\\s*([[:alnum:]._]+).*",
    "\\1", grep("^\\s*[[:alnum:]._]+\\s*(<-|=)", lines, value = TRUE)
  )
  
  res <- length(objs) == 0L
  
  if(res)
    cli_warn(neg_msg, normalizePath(FILE))
  
  return(!res)
}
