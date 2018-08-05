Sinew News
================

## 0.3.5.1

<details>

<summary>Added</summary>

  - `NEWS.md` file to track changes to the
    package.
  - [pretty\_namespaces](https://metrumresearchgroup.github.io/sinew/pretty-namespace.html)
      - Function that autoappends namespace to functions in script by
        searchpath
    order.
  - [create\_yml](https://metrumresearchgroup.github.io/sinew/using-sinewconfig-yml.html)
      - Functions that creates `_sinewconfig.yml` in the current project
        root directory, and updates
    `.Rbuildignore`.

</details>

<details>

<summary>Updated</summary>

  - [makeImport](https://metrumresearchgroup.github.io/sinew/makeimport.html)
      - Writes directly to DESCRIPTION
    file.
  - [untangle](https://metrumresearchgroup.github.io/sinew/untangle.html)
      - Create the `body.R` file in the working directory instead of the
        directory where the functions are created.
      - Naming scheme for files is updated to replace all `.` in
        function names to
    `_`.

</details>

<details>

<summary>Removed</summary>

  - [makeImport](https://metrumresearchgroup.github.io/sinew/makeimport.html)
      - does not create namespace output

</details>

## 0.3.5.2

<details>

<summary>Updated</summary>

  - Fixed bug in `pretty_namespace` when global env is empty.

</details>

## 0.3.6

<details>

<summary>Updated</summary>

  - Fixed bugs in `pretty_namespace`
      - correct shift in lines with multiple namespaces
      - search only exported namespace functions

</details>
