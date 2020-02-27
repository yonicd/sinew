<<<<<<< HEAD
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/sinew)](https://cran.r-project.org/package=sinew)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/sinew)
[![Travis-CI Build Status](https://travis-ci.org/metrumresearchgroup/sinew.svg?branch=master)](https://travis-ci.org/metrumresearchgroup/sinew)
[![Covrpage Summary](https://img.shields.io/badge/covrpage-Last_Build_2018_11_26-brightgreen.svg)](https://github.com/metrumresearchgroup/sinew/tree/master/tests/README.md)
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
=======
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sinew)](https://cran.r-project.org/package=sinew)
[![](https://cranlogs.r-pkg.org/badges/sinew)](https://cran.r-project.org/package=sinew)
[![Travis-CI Build
Status](https://travis-ci.org/metrumresearchgroup/sinew.svg?branch=master)](https://travis-ci.org/metrumresearchgroup/sinew)
[![Coverage
Status](https://img.shields.io/codecov/c/github/metrumresearchgroup/sinew/master.svg)](https://codecov.io/github/metrumresearchgroup/sinew?branch=master)
[![Covrpage
Summary](https://img.shields.io/badge/covrpage-Initialized-orange.svg)](https://github.com/metrumresearchgroup/sinew/tree/master/tests/README.md)
>>>>>>> origin/master


# Sinew

Sinew is a `R` package that facilitates `R` package documentation and namespaces managment for developers of all levels. 

## tldr

What can it do?

  - Programatically attach namespaces to functions in `R` and `Rmd` script
  - Generate `Roxygen2` skeletons populated with information scraped from within functions
  - Populate the `Imports` field of the `DESCRIPTION` file.

## Installation

### CRAN

```r
install.packages('sinew')
```

### DEV

```r
install.packages('remotes')
remotes::install_github('metrumresearchgroup/sinew')
```

## More Details

At the core of creating reproducible research is the daunting process of documentation, and working with `R` presents no exception. The standardized `R` package documentation is part of the appeal for new (and experienced) users, where concise, clear instructions are cherished. Packages are great for reusing scripts in a consistent fashion and for developers, package creation helps organizing and maintaining their ideas.

The barrier to entry for package maintenance and documentation, however may be too high for `R` users embarking on the task of package creation, thereby discouraging them from development. The out-of-the-box [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) skeleton supplied by `RStudio` gives the bare bones road map of what should be part of function documentation.

The goal of `sinew` is to automate nearly all of the manual tasks needed to document functions, properly set up the import fields for oxygenation, and make it easier to attain documentation consistency across functions and packages.

Once the initial documentation is set, `sinew` can be used as part of the natural maintenance workflow to automatically change and append updated parameters, definitions, defaults, add namespacing to functions and dependencies, making your documentation great - again!

`sinew` is built for both command line and interactive users through a Shiny gadget that can be called from the addin menu. The addin can be used on any function either installed, in the search path, one you just wrote, or one your need to update.
