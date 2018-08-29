[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/sinew)](https://cran.r-project.org/package=sinew)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/sinew)
[![Travis-CI Build Status](https://travis-ci.org/metrumresearchgroup/sinew.svg?branch=master)](https://travis-ci.org/metrumresearchgroup/sinew)
[![Covrpage Summary](https://img.shields.io/badge/covrpage-Last_Build_2018_08_29-brightgreen.svg)](https://github.com/metrumresearchgroup/sinew/tree/master/tests/README.md)
# Sinew

Sinew is a R package that generates a roxygen2 skeleton populated with information scraped from the function script.

## Installation
```r
install.packages("sinew")
# or:
# devtools::install_github('metrumresearchgroup/sinew')
```

## Online User Manual Gitbook

A gitbook is maintained as the user manual for the package, you can access it here:

https://metrumresearchgroup.github.io/sinew/

## Motivation

How can `sinew` help you?

`sinew` automates tasks that are part of R package documentation and maintance in order to help developers consistently create robust roxygen2 documentation and pass `R CMD check --as-cran`.

### Quick Turn Around

Two common scenarios arise in package development 

  - You start a new project and open a file to develop your idea. Many functions later at the end of the day you look up and you have created a mess of a file. 
  
![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/Guy-Tangled-in-Lights.jpg?raw=true)

  - You recieve a mammoth 10,000 line uncommented file to decipher for QC - good luck. 
  
![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/xmaslights.gif?raw=true)
  
`Sinew` can help turn the around that headache into a CRAN ready package in a few short steps

  1. Open a package project in Rstudio
  2. Place the file in the project root
  3. Run `untangle` on the large script with the destination directory `./R`. This will separate the core functions in the body into single function files (named as the function) and keep the body in `body.R`. 
  4. Run `pretty_namespace` to append any missing namespaces in the function scripts.
  5. Run `makeOxyFile` with the path set to `./R`.
  6. Run `makeImport` to create the correct `Imports` in the `DESCRIPTION` file and paste it in the the DESCRIPTION file. 

This should get you far enough to make the impossible problem of understanding what is in that file to a manageable task, with the added benefit of producing a new package ready for distribution.

### Working example

Lets use a reproducible example - **The goal is to convert raw script in a file called [test.R](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/test/test.R) into a package.**

The file includes two functions `yy` and `zz` and some general script that uses them

```r
#some comment
yy <- function(a=4){
  head(runif(10),a)
  # a comment
}

zz <- function(v=10,a=8){
  head(runif(v),a)
}


yy(6)

zz(30,3)
```

To start we initialize a `R` subdirectory and a DESCRIPTION file.

```r
dir.create('R')
devtools::create_description()
```

#### Untangling Files
  
One of the first tasks for new developers is to move from long scripts that are intertwined with functions and body code into single function files in a R subdirectory and a clean body script that is easier to read. 

This task is probably a non-starter if you have more than a few hundered lines of code. This is where [untangle](https://metrumresearchgroup.github.io/sinew/untangle.html) can save you time. `untangle` will separate the long script into single function files in a subdirectory and keep the body script intact. 

```r
untangle(file = 'test.R',dir.out = 'R')
```

![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/untangle_test.gif?raw=true)

As we can see we got three new files. 
    - `body.R` in the working directory
    - `yy.R` in the `R` subdirectory
    - `zz.R` in the `R` subdirectory

#### Namespacing

It has become common practice to  use the namespace in function calls, and it is obligatory in order to pass a cran check. But, not everyone does it and if you're not use to it, it's a pain to go back and update your script.

This is where [pretty_namespace](https://metrumresearchgroup.github.io/sinew/pretty-namespace.html) comes in. This function will go through your script and attach namespaces for you, with the same logic as the search path. 

```r
pretty_namespace('R',overwrite = TRUE)
```

![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/pretty_ns_test.gif?raw=true)

So now we have separate files with functions appropriatly associated with namespaces, and now we can add roxygen2 headers.

#### Documentation

Now we are ready to create the function documentation using roxygen2. We use [makeOxygen](file:///Users/jonathans/projects/sinew/Miscellaneous/docs/makeoxygen.html) to create a skeleton for [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) documentation. This function returns a skeleton that includes title, description, return, import and other fields populated with information scraped from the function script. We can also run `makeOxygen` in batch mode using [makeOxyFile](file:///Users/jonathans/projects/sinew/Miscellaneous/docs/makeoxyfile.html).

```r
makeOxyFile('R',overwrite = TRUE)
```

![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/oxy_test.gif?raw=true)

Lets see what we got back in `zz.R`:

```r
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param v PARAM_DESCRIPTION, Default: 10
#' @param a PARAM_DESCRIPTION, Default: 8
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{head}}
#'  \code{\link[stats]{runif}}
#' @rdname zz
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils head
#' @importFrom stats runif
zz <- function(v=10,a=8){
  utils::head(stats::runif(v),a)
}
```

The premise of `makeOxygen` is to expand on the default skeleton in RStudio, so basic fields are in the output by default. Each field is given with a relevant placeholder giving a hint what is expected. The following is the meat add to these bones:

  - param default values:
    - If a default value is set for a function parameter it will be added to the end  `@param` line.
  - import/importFrom
    - The package scrapes the script with `makeImport` looking for declared namespaces to create the proper calls for `@import` and `@importFrom` which are placed at the bottom of the output.
  - seealso
    - linking to other packages is also taken care of when adding the field `@seealso`. Any functions that are included in `@importFrom` will have a link to them by default.

####  DESCRIPTION

It is also important to update the package description file `Imports` field. This can be done for you with [makeImport](file:///Users/jonathans/projects/sinew/Miscellaneous/docs/makeimport.html#description), by either creating a new `Imports` field or updating an existing one.

```r
makeImport('R',format = 'description',desc_loc = '.')
```

![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/makeimport_test.gif?raw=true)

#### Update documentation

An important part of maintaining a package is keeping the documentation updated. Using [moga](file:///Users/jonathans/projects/sinew/Miscellaneous/docs/moga.html) we can achieve this painlessly. `moga` runs the same underlying script as `makeOxygen` but appends new information found into the current roxygen2 header instead of creating a new one.

Lets say we updated `yy.R` to include another param and used another function from the `stats` package. So the roxygen2 header is now out of synch with the current script.

```r
#some comment
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a numeric, set the head to trim from random unif Default: 4
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{head}}
#'  \code{\link[stats]{runif}}
#' @rdname yy
#' @export 
#' @author Jonathan Sidi
#' @importFrom utils head
#' @importFrom stats runif
yy <- function(a=4,b=2){
  x <- utils::head(stats::runif(10*b),a)
  stats::quantile(x,probs=.95)
  # a comment
}
```

```r
moga('R/yy.r',overwrite = TRUE)
```

![](https://github.com/metrumresearchgroup/sinew/blob/gh-pages/Miscellaneous/moga_test.gif?raw=true)

#### Oxygenize and Check

##### Oxygenize

```r
devtools::document()
Updating test documentation
Loading test
First time using roxygen2. Upgrading automatically...
Updating roxygen version in /Users/jonathans/projects/sinew/Miscellaneous/test/DESCRIPTION
Writing NAMESPACE
Writing yy.Rd
Writing zz.Rd
```

##### R CMD Check

```r
devtools::use_mit_license()
* Updating license field in DESCRIPTION.
* Creating `LICENSE` from template.

devtools::check(cran = TRUE)
Updating test documentation
Loading test
Setting env vars -------------------------------------------------------
CFLAGS  : -Wall -pedantic
CXXFLAGS: -Wall -pedantic
Building test ----------------------------------------------------------
'/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
  --no-environ --no-save --no-restore --quiet CMD build  \
  '/Users/jonathans/projects/sinew/Miscellaneous/test'  \
  --no-resave-data --no-manual 

* checking for file ‘/Users/jonathans/projects/sinew/Miscellaneous/test/DESCRIPTION’ ... OK
* preparing ‘test’:
* checking DESCRIPTION meta-information ... OK
* checking for LF line-endings in source and make files
* checking for empty or unneeded directories
* building ‘test_0.0.0.9000.tar.gz’

Setting env vars -------------------------------------------------------
_R_CHECK_CRAN_INCOMING_ : FALSE
_R_CHECK_FORCE_SUGGESTS_: FALSE
Checking test ----------------------------------------------------------
'/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
  --no-environ --no-save --no-restore --quiet CMD check  \
  '/var/folders/4_/xhs9__yd49l4v4j4wdg9f0wr0000gp/T//Rtmp1KA1SX/test_0.0.0.9000.tar.gz'  \
  --as-cran --timings --no-manual 

* using log directory ‘/private/var/folders/4_/xhs9__yd49l4v4j4wdg9f0wr0000gp/T/Rtmp1KA1SX/test.Rcheck’
* using R version 3.3.3 (2017-03-06)
* using platform: x86_64-apple-darwin13.4.0 (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘test/DESCRIPTION’ ... OK
* this is package ‘test’ version ‘0.0.0.9000’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘test’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE

Status: OK

R CMD check results
0 errors | 0 warnings | 0 notes
```

##### Does it work?
  
```r
test::yy()
      95% 
0.4104221 

test::zz()
[1] 0.84515305 0.72393508 0.74036634 0.03006037 0.67631049 0.71084114
[7] 0.86142639 0.12371146
```
