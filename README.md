# Sinew

Sinew is a R package that generates a Roxygen skeleton populated with information scraped from the function script.

## Functionality

  - [makeOxygen](https://github.com/metrumresearchgroup/sinew#makeoxygen): Create skeleton for Roxygen2 documentation populated with information scraped from within the package function scripts.
  - [makeImport](https://github.com/metrumresearchgroup/sinew#makeimport): Create import calls for DESCRIPTION, NAMESPACE and Roxygen2
  - [makeDictionary](https://github.com/metrumresearchgroup/sinew#makedictionary): Create an R file of all the unique roxygen parameter fields in a package R subdirectory.

## makeOxygen

Function that returns the skeleton for [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) documentation including title, description, return, import and other fields populated with information scraped from the function script. 

The addin `createOxygen` uses highlighted text in the active document of  RStudio as the object argument.

### Basic Usage

makeOxygen is the main function in the package. Running the default setting returns a skeleton with minimal required fields to run `devtools::check(build_args = '--as-cran')`: title, description, and param. 

#### Adding Some Meat to the Bones...

The added value of sinew is that it scrapes the script and fills in many important holes in the documentation:

  - param default values:
    - if a default value is set for a function parameter it will be added to the end  `@param` line.
  - import/importFrom
    - It is assumed that the developer is abiding by the CRAN rules and uses the proper namespace syntax `package::function` when calling functions in the script. The package scrapes the script with `makeImport` to create the proper calls for `@import` and `@importFrom` which are placed at the bottom of the output. The user has control the number of functions that are listed in `importFrom package function1 [ function2 ...]` until only `@import package` is returned (more below).
  - seealso
    - linking to other packages is also taken care of when adding the field `@seealso`. Any functions that are included in `@importFrom` will have a link to them by default.
    
Examples showing different parameter specification in makeOxygen

#### Basic

```r
makeOxygen(lm)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param formula PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param subset PARAM_DESCRIPTION
#' @param weights PARAM_DESCRIPTION
#' @param na.action PARAM_DESCRIPTION
#' @param method PARAM_DESCRIPTION, Default: 'qr'
#' @param model PARAM_DESCRIPTION, Default: TRUE
#' @param x PARAM_DESCRIPTION, Default: FALSE
#' @param y PARAM_DESCRIPTION, Default: FALSE
#' @param qr PARAM_DESCRIPTION, Default: TRUE
#' @param singular.ok PARAM_DESCRIPTION, Default: TRUE
#' @param contrasts PARAM_DESCRIPTION, Default: NULL
#' @param offset PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @importFrom stats model.frame
```

#### Add_fields

Control over which roxygen2 fields are added to the header is passed through `add_fields`.

```r
makeOxygen(colourpicker:::colourPickerGadget,add_fields = c('export','details','examples'))
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param numCols PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @export
#' @details DETAILS
#' @examples
#' EXAMPLE1 
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom grDevices colours
#' @importFrom shiny addResourcePath dialogViewer runGadget shinyApp
#' @importFrom shinyjs useShinyjs extendShinyjs toggleState disable onclick alert
#' @importFrom utils packageVersion
```
#### Cut

Passing `cut` to makeOxygen to return `import package ` instead of `importFrom package funtion1 [function2 ...]` for packages that call more than the value assigned to cut

```r
> makeOxygen(colourpicker:::colourPickerGadget,add_fields = c('export','details','examples'),cut=3)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param numCols PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @export
#' @details DETAILS
#' @examples
#' EXAMPLE1 
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom grDevices colours
#' @import shiny
#' @import shinyjs
#' @importFrom utils packageVersion
```

#### Seealso

When calling `addfields('seealso')` the function will give a guess of which functions to add conditional on what value cut it set to. That is any function returned with importFrom will also have a seealso link created for it

```r
> makeOxygen(shinyHeatmaply:::heatmaplyGadget,cut=3,add_fields = 'seealso')
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param obj PARAM_DESCRIPTION
#' @param plotHeight PARAM_DESCRIPTION, Default: 800
#' @param viewerType PARAM_DESCRIPTION, Default: 'paneViewer'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @seealso
#'  \code{\link[DT]{dataTableOutput}},\code{\link[DT]{renderDataTable}}
#'  \code{\link[tools]{file_path_sans_ext}}
#'  \code{\link[xtable]{xtable}}
#' @importFrom DT dataTableOutput renderDataTable
#' @import heatmaply
#' @import htmltools
#' @import plotly
#' @import shiny
#' @import stats
#' @importFrom tools file_path_sans_ext
#' @importFrom xtable xtable

```

#### Dictionary

A dictionary is an R file produced with [makeDictionary](https://github.com/metrumresearchgroup/sinew#makedictionary). This R file contains  all the unique roxygen parameter fields in a package R subdirectory.

```r
makeOxygen(tabular,add_fields = c('examples','export'))

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param header PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @examples 
#' EXAMPLE1 
#'
#' @export 

dict_loc='https://raw.githubusercontent.com/metrumresearchgroup/sinew/master/man-roxygen/Dictionary-1.R'

makeOxygen(tabular,use_dictionary = dict_loc,add_fields = c('examples','export'))

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df data.frame to convert to table
#' @param header boolean to control if header is created from names(df), Default: TRUE
#' @param ...
#' @return OUTPUT_DESCRIPTION
#' @examples 
#' EXAMPLE1 
#'
#' @export 
```

#### Data.frames

makeOxygen also creates documentation for data.frames and tibble objects

```r
makeOxygen(iris)
#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{\code{Sepal.Length}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Sepal.Width}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Petal.Length}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Petal.Width}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{integer COLUMN_DESCRIPTION} 
#'}
"iris"
```

## makeImport

When you are building a package to submit to cran and you need to have namespace calls for any function that is being imported. It is a pain to manually parse through the code looking for all the `*::*` and writing it in the roxygen header. This function does that for you. 

You can write normally your script with the namespace calls and in the end run the function and you can paste the output into the header. (or use it as part of `makeOxygen`)

The function is written to work on single files or whole directories, like a package R subdirectory.

The output can be set to return the format needed for either an roxygen header, NAMESPACE or the DESCRIPTION

### DESCRIPTION

```r
makeImport(script=list.files('R',full.names = T),print = T,format = 'description')
Imports: rstudioapi,utils
```

### NAMESPACE

```r
makeImport(script=list.files('R',full.names = T),print = T,format = 'namespace')
 
importFrom(rstudioapi,getActiveDocumentContext)
importFrom(rstudioapi,insertText)
importFrom(utils,installed.packages)
```

### Roxygen

```r
makeImport(script=list.files('R',full.names = T),print = T,format = 'oxygen')
 
R/importAddin.R
#' @importFrom rstudioapi getActiveDocumentContext
 
R/makeImport.R
#' @importFrom utils installed.packages
 
R/makeOxygen.R

 
R/makeSeeAlso.R

 
R/oxygenAddin.R
#' @importFrom rstudioapi getActiveDocumentContext insertText
```

#### importFrom cutoff

Setting cut to a value allows for control of how many functions to list in a package before concatenating the `importFrom` to an `import`. This is useful when there are many functions being used throughout the package from the same library and it is practically the same as just importing the whole library

```r
makeImport(script='R/oxygenAddin.R',print = T,format = 'oxygen')

R/oxygenAddin.R
#' @importFrom rstudioapi getActiveDocumentContext insertText
 
makeImport(script='R/oxygenAddin.R',print = T,format = 'oxygen',cut=2)
 
R/oxygenAddin.R
#' @import rstudioapi
```

### makeDictionary

This function takes the idea of roxygen templates, but repurposes their use. It creates an R file of all the unique roxygen parameter fields in a package R subdirectory. This serves a few functions:

  - Creates a general template for regular use with Roxygen2
  - Simple way to check that there are no redundant parameter descriptions and that they are consistent.
  - When present, this is used internally with **ls_param** to call parameter descriptions in bulk from a centralized template to populate makeOxygen skeletons.
  
For example in the [man-roxygen](https://github.com/metrumresearchgroup/sinew/tree/master/man-roxygen) there is a Dictionary-1.R file that was created by **makeDictionary**. Using **ls_param** a query is run on the dictionary to return the param fields that intersect with the formals call to the functions. 

```r
dict_loc='https://raw.githubusercontent.com/metrumresearchgroup/sinew/master/man-roxygen/Dictionary-1.R'

ls_param(makeOxygen,dictionary = dict_loc)

#' @param obj function or name of function
#' @param add_default boolean to add defaults values to the end of the PARAM fields, Default: TRUE
#' @param add_fields character vector to add additional roxygen fields, Default: NULL
#' @param print boolean print output to console, Default: TRUE
#' @param ... 

names(formals(makeOxygen))
[1] "obj"         "add_default" "add_fields"  "print"       "..." 
```