Sinew News
================

## 0.3.5.1

<details>
<summary>
Added
</summary>

-   `NEWS.md` file to track changes to the package.
-   [pretty\_namespaces](https://yonicd.github.io/sinew/reference/pretty_namespace.html)
    -   Function that autoappends namespace to functions in script by
        searchpath order.
-   [create\_yml](https://yonicd.github.io/sinew/reference/create_yml.html)
    -   Functions that creates `_sinewconfig.yml` in the current project
        root directory, and updates `.Rbuildignore`.

</details>
<details>
<summary>
Updated
</summary>

-   [makeImport](https://yonicd.github.io/sinew/reference/make_import.html)
    -   Writes directly to DESCRIPTION file.
-   [untangle](https://yonicd.github.io/sinew/reference/untangle.html)
    -   Create the `body.R` file in the working directory instead of the
        directory where the functions are created.
    -   Naming scheme for files is updated to replace all `.` in
        function names to `_`.

</details>
<details>
<summary>
Removed
</summary>

-   [makeImport](https://yonicd.github.io/sinew/reference/make_import.html)
    -   does not create namespace output

</details>

## 0.3.5.2

<details>
<summary>
Updated
</summary>

-   Fixed bug in `pretty_namespace` when global env is empty.

</details>

## 0.3.6

<details>
<summary>
Added
</summary>

-   `pretty_namespace`
    -   add console summary output for changes in file

</details>
<details>
<summary>
Updated
</summary>

-   Fixed bugs in `pretty_namespace`
    -   correct shift in lines with multiple namespaces
    -   search only exported namespace functions
    -   function split up into smaller functions found in pretty\_utils

</details>

## 0.4.0

<details>
<summary>
Added
</summary>

-   `make_force_packages`
    -   create lists for usage with `pretty_namespace` force argument.

</details>
