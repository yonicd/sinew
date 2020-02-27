[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sinew)](https://cran.r-project.org/package=sinew)
[![](https://cranlogs.r-pkg.org/badges/sinew)](https://cran.r-project.org/package=sinew)
[![Travis-CI Build
Status](https://travis-ci.org/metrumresearchgroup/sinew.svg?branch=master)](https://travis-ci.org/metrumresearchgroup/sinew)
[![Coverage
Status](https://img.shields.io/codecov/c/github/metrumresearchgroup/sinew/master.svg)](https://codecov.io/github/metrumresearchgroup/sinew?branch=master)
[![Covrpage
Summary](https://img.shields.io/badge/covrpage-Initialized-orange.svg)](https://github.com/metrumresearchgroup/sinew/tree/master/tests/README.md)


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
