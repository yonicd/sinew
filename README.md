[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/sinew)](https://cran.r-project.org/package=sinew)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/sinew)

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

## Updates
 Thanks to some great PRs and discussions with Anton Grishin (@grishin1), Lorenzo Busseto (@lbusett) and Devin Pastoor (@dpastoor) the package has improved API and an interactive UI. 
 
  - Interactive addin to access and create documentation for any function either on disk, searchpath or sourced on demand. (@grishin1)
  
  ![](https://github.com/metrumresearchgroup/sinew/blob/master/Miscellaneous/interactiveAddin.gif?raw=true)
  
  - Improved under-the-hood and API for makeOxygen calls (@lbusett)
  - Addition of functionality similar to `knitr::opts` called `sinew::sinew_opts`
    - can get, set, and append to global package options for a R session
    - package options that can be set for a session are: 
      - add_fields: the fields in addition to the default documentation fields to add to sinew outputs (Default: details, examples, seealso, rdname, export, author)
      - `sinew_opts$append()` can be useful here to grow the character vector of add_fields instead of replacing it with `sinew_opts$set()`
      - The default values for any of the following roxygen2 fields can be set for the session: author, backref, concept, describeIn, details, example, examples, export, family, field, format, importClassesFrom, importMethodsFrom, include, inherit, inheritDotParams, inheritSection, keywords, name, rdname, references, section, source, slot, template, templateVar, useDynLib
  - yml file that is placed in the package parent directory. This can be used to define any values for sinew_opts and they will override the sinew defaults. (@dpastoor)
  - `sinew::moga` (make oxygen great again!). This function will cross check the current documentation of an R script and the current script itself and update/add any parameters/defaults/imports discrepancies.
  - `sinew::makeOxyFile` (makeOxygen for a file or batchmode for a directory) (@grishin1)
