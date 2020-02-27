#' @title Create a toy package
#' @description Create a toy package to test sinew functions
#' @param pkg_dir character, path to create the package, Default: file.path(tempdir(), "pkg")
#' @param example_file character, path to script to place in data-raw, Default: system.file("example.R", package = "sinew")
#' @return pkg_dir
#' @details A package is created with the name of the leaf directory on pkg_dir path. 
#' By default the package contains:
#' 
#' * DESCRIPTION
#' * LICENSE
#' * LICENSE.md
#' * NAMESPACE
#' * R
#'   * yy.R
#'   * zz.R
#' * data-raw
#'   * DATASET.R
#'   * body.R
#'   * system.file('example.R', package = 'sinew')
#' 
#' @examples 
#' 
#' pkg_dir <- file.path(tempdir(),'pkg')
#' 
#' mock_package(pkg_dir = pkg_dir)
#'  
#' list.files(pkg_dir, recursive = TRUE)
#' 
#' @author Jonathan Sidi
#' @importFrom usethis create_package use_data_raw use_mit_license use_roxygen_md
#' @importFrom withr with_dir
#' @rdname mock_package
#' @concept utility
#' @export
mock_package <- function(pkg_dir = file.path(tempdir(),'pkg'), 
                         example_file = system.file('example.R', package = 'sinew')){
  
  usethis::create_package(path = pkg_dir, open = FALSE)
  withr::with_dir(pkg_dir, usethis::use_data_raw(open = FALSE))
  withr::with_dir(pkg_dir, usethis::use_mit_license(name = "John Doe"))
  withr::with_dir(pkg_dir, usethis::use_roxygen_md())
  
  untangle(
    file = example_file, 
    dir.out = file.path(pkg_dir, 'R'), 
    dir.body = file.path(pkg_dir, 'data-raw')
  )

  invisible(pkg_dir)
}