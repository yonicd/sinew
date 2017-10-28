Sinew News
================

0.3.5.1
-------

### Added

-   `NEWS.md` file to track changes to the package.
-   [pretty\_namespaces](https://metrumresearchgroup.github.io/sinew/pretty-namespace.html)
    -   Function that autoappends namespace to functions in script by searchpath order.
-   [create\_yml](https://metrumresearchgroup.github.io/sinew/using-sinewconfig-yml.html)
    -   Functions that creates `_sinewconfig.yml` in the current project root directory, and updates `.Rbuildignore`.

### Updated

-   [makeImport](https://metrumresearchgroup.github.io/sinew/makeimport.html)
    -   Writes directly to DESCRIPTION file.
-   [untangle](https://metrumresearchgroup.github.io/sinew/untangle.html)
    -   Create the `body.R` file in the working directory instead of the directory where the functions are created.
    -   Naming scheme for files is updated to replace all `.` in function names to `_`.

### Removed

-   [makeImport](https://metrumresearchgroup.github.io/sinew/makeimport.html)
    -   does not create namespace output
