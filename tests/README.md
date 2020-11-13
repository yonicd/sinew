Tests and Coverage
================
13 November, 2020 01:22:33

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
| :---------------------------------------------------- | :----------: |
| sinew                                                 |    17.61     |
| [R/check\_attach.R](../R/check_attach.R)              |     0.00     |
| [R/create\_yml.R](../R/create_yml.R)                  |     0.00     |
| [R/get\_oxy.R](../R/get_oxy.R)                        |     0.00     |
| [R/interOxyAddIn.R](../R/interOxyAddIn.R)             |     0.00     |
| [R/ls\_param.R](../R/ls_param.R)                      |     0.00     |
| [R/makeDictionary.R](../R/makeDictionary.R)           |     0.00     |
| [R/makeOxyFile.R](../R/makeOxyFile.R)                 |     0.00     |
| [R/makeOxygen.R](../R/makeOxygen.R)                   |     0.00     |
| [R/moga.R](../R/moga.R)                               |     0.00     |
| [R/opts\_complete.R](../R/opts_complete.R)            |     0.00     |
| [R/oxygenAddin.R](../R/oxygenAddin.R)                 |     0.00     |
| [R/pretty\_addin\_utils.R](../R/pretty_addin_utils.R) |     0.00     |
| [R/pretty\_addin.R](../R/pretty_addin.R)              |     0.00     |
| [R/pretty\_rmd.R](../R/pretty_rmd.R)                  |     0.00     |
| [R/pretty\_sinew.R](../R/pretty_sinew.R)              |     0.00     |
| [R/rm\_oxylines.R](../R/rm_oxylines.R)                |     0.00     |
| [R/rmOxygen.R](../R/rmOxygen.R)                       |     0.00     |
| [R/something.R](../R/something.R)                     |     0.00     |
| [R/untangle\_examples.R](../R/untangle_examples.R)    |     0.00     |
| [R/untangle.R](../R/untangle.R)                       |     0.00     |
| [R/opts.R](../R/opts.R)                               |     5.88     |
| [R/zzz.R](../R/zzz.R)                                 |    11.11     |
| [R/pretty\_utils.R](../R/pretty_utils.R)              |    51.96     |
| [R/makeImport.R](../R/makeImport.R)                   |    62.71     |
| [R/pretty\_namespace.R](../R/pretty_namespace.R)      |    83.33     |
| [R/tabular.R](../R/tabular.R)                         |    88.89     |
| [R/make\_seealso.R](../R/make_seealso.R)              |    94.12     |
| [R/prettify.R](../R/prettify.R)                       |    94.74     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                      |  n |  time | error | failed | skipped | warning | icon |
| :---------------------------------------- | -: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test-pretty.R](testthat/test-pretty.R)   | 14 | 0.803 |     0 |      0 |       1 |       0 | 🔶    |
| [test-seealso.R](testthat/test-seealso.R) |  3 | 0.007 |     0 |      0 |       0 |       0 |      |
| [test-tabular.R](testthat/test-tabular.R) |  3 | 0.007 |     0 |      0 |       0 |       0 |      |

<details open>

<summary> Show Detailed Test Results </summary>

| file                                            | context      | test                                           | status  | n |  time | icon |
| :---------------------------------------------- | :----------- | :--------------------------------------------- | :------ | -: | ----: | :--- |
| [test-pretty.R](testthat/test-pretty.R#L9)      | pretty       | switches: force                                | SKIPPED | 1 | 0.018 | 🔶    |
| [test-pretty.R](testthat/test-pretty.R#L26)     | pretty       | switches: ignore                               | PASS    | 1 | 0.354 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup files: length                            | PASS    | 1 | 0.001 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup files: class                             | PASS    | 1 | 0.132 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup files: names                             | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup file: length                             | PASS    | 1 | 0.001 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup file: class                              | PASS    | 1 | 0.013 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup file: names                              | PASS    | 1 | 0.001 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup text: length                             | PASS    | 1 | 0.001 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup text: class                              | PASS    | 1 | 0.006 |      |
| [test-pretty.R](testthat/test-pretty.R#)        | pretty       | setup text: names                              | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L69)     | pretty       | full text: txt                                 | PASS    | 2 | 0.120 |      |
| [test-pretty.R](testthat/test-pretty.R#L80_L82) | pretty       | full file: file                                | PASS    | 1 | 0.152 |      |
| [test-seealso.R](testthat/test-seealso.R#)      | make seealso | cutoff: less than cutoff                       | PASS    | 1 | 0.001 |      |
| [test-seealso.R](testthat/test-seealso.R#)      | make seealso | no cutoff: no elements                         | PASS    | 1 | 0.001 |      |
| [test-seealso.R](testthat/test-seealso.R#)      | make seealso | no cutoff: simple call                         | PASS    | 1 | 0.005 |      |
| [test-tabular.R](testthat/test-tabular.R#)      | tabular      | convert dataframe to tabular header: length    | PASS    | 1 | 0.001 |      |
| [test-tabular.R](testthat/test-tabular.R#)      | tabular      | convert dataframe to tabular header: class     | PASS    | 1 | 0.005 |      |
| [test-tabular.R](testthat/test-tabular.R#)      | tabular      | convert dataframe to tabular no header: length | PASS    | 1 | 0.001 |      |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| 🛑      | ⚠️      | 🔶       |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                             |                                                                                                                                                                                                                                                               |
| :------- | :-------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Version  | R version 4.0.3 (2020-10-10)      |                                                                                                                                                                                                                                                               |
| Platform | x86\_64-apple-darwin17.0 (64-bit) | <a href="https://github.com/yonicd/sinew/commit/2f0633f2092b41bb9ebf508fa4167400ec9a52e0/checks" target="_blank"><span title="Built on Github Actions">![](https://github.com/metrumresearchgroup/covrpage/blob/actions/inst/logo/gh.png?raw=true)</span></a> |
| Running  | macOS Catalina 10.15.7            |                                                                                                                                                                                                                                                               |
| Language | en\_US                            |                                                                                                                                                                                                                                                               |
| Timezone | UTC                               |                                                                                                                                                                                                                                                               |

| Package  | Version |
| :------- | :------ |
| testthat | 3.0.0   |
| covr     | 3.3.2   |
| covrpage | 0.0.71  |

</details>

<!--- Final Status : skipped/warning --->
