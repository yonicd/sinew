Tests and Coverage
================
26 November, 2018 06:48:43

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by [covrpage](https://github.com/yonicd/covrpage).

Coverage
--------

Coverage summary is created using the [covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
|:------------------------------------------------------|:------------:|
| sinew                                                 |     10.16    |
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
| [R/something.R](../R/something.R)                     |     0.00     |
| [R/untangle\_examples.R](../R/untangle_examples.R)    |     0.00     |
| [R/untangle.R](../R/untangle.R)                       |     0.00     |
| [R/pretty\_utils.R](../R/pretty_utils.R)              |     3.80     |
| [R/opts.R](../R/opts.R)                               |     5.88     |
| [R/zzz.R](../R/zzz.R)                                 |     13.33    |
| [R/make\_import.R](../R/make_import.R)                |     50.88    |
| [R/pretty\_namespace.R](../R/pretty_namespace.R)      |     54.55    |
| [R/make\_seealso.R](../R/make_seealso.R)              |     58.33    |
| [R/prettify.R](../R/prettify.R)                       |     59.65    |
| [R/rmOxygen.R](../R/rmOxygen.R)                       |     95.45    |
| [R/check\_attach.R](../R/check_attach.R)              |    100.00    |
| [R/tabular.R](../R/tabular.R)                         |    100.00    |

<br>

Unit Tests
----------

Unit Test summary is created using the [testthat](https://github.com/r-lib/testthat) package.

| file                                                 |    n|   time|  error|  failed|  skipped|  warning|
|:-----------------------------------------------------|----:|------:|------:|-------:|--------:|--------:|
| [test-check\_attach.R](testthat/test-check_attach.R) |    2|  0.002|      0|       0|        0|        0|
| [test-pretty.R](testthat/test-pretty.R)              |   12|  0.370|      0|       0|        0|        0|
| [test-rm.R](testthat/test-rm.R)                      |    4|  0.012|      0|       0|        0|        0|
| [test-seealso.R](testthat/test-seealso.R)            |    2|  0.003|      0|       0|        0|        0|
| [test-tabular.R](testthat/test-tabular.R)            |    3|  0.005|      0|       0|        0|        0|

<details closed> <summary> Show Detailed Test Results </summary>

| file                                                  | context       | test                                           | status |    n|   time|
|:------------------------------------------------------|:--------------|:-----------------------------------------------|:-------|----:|------:|
| [test-check\_attach.R](testthat/test-check_attach.R#) | check\_attach | test check attach: already loaded              | PASS   |    1|  0.001|
| [test-check\_attach.R](testthat/test-check_attach.R#) | check\_attach | test check attach: not already loaded          | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#L16)           | pretty        | switches: force                                | PASS   |    2|  0.187|
| [test-pretty.R](testthat/test-pretty.R#L27)           | pretty        | switches: ignore                               | PASS   |    1|  0.172|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup files: length                            | PASS   |    1|  0.002|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup files: class                             | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup files: names                             | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup file: length                             | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup file: class                              | PASS   |    1|  0.002|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup file: names                              | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup text: length                             | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup text: class                              | PASS   |    1|  0.001|
| [test-pretty.R](testthat/test-pretty.R#)              | pretty        | setup text: names                              | PASS   |    1|  0.001|
| [test-rm.R](testthat/test-rm.R#L17_L20)               | remove oxygen | rm valid actions: no show file                 | PASS   |    1|  0.004|
| [test-rm.R](testthat/test-rm.R#L27_L30)               | remove oxygen | rm valid actions: show file                    | PASS   |    1|  0.004|
| [test-rm.R](testthat/test-rm.R#L48_L51)               | remove oxygen | rm invalid actions: extension                  | PASS   |    1|  0.002|
| [test-rm.R](testthat/test-rm.R#L57_L60)               | remove oxygen | rm invalid actions: path                       | PASS   |    1|  0.002|
| [test-seealso.R](testthat/test-seealso.R#)            | make seealso  | cutoff: less than cutoff                       | PASS   |    1|  0.001|
| [test-seealso.R](testthat/test-seealso.R#)            | make seealso  | no cutoff: no elements                         | PASS   |    1|  0.002|
| [test-tabular.R](testthat/test-tabular.R#)            | tabular       | convert dataframe to tabular header: length    | PASS   |    1|  0.002|
| [test-tabular.R](testthat/test-tabular.R#)            | tabular       | convert dataframe to tabular header: class     | PASS   |    1|  0.002|
| [test-tabular.R](testthat/test-tabular.R#)            | tabular       | convert dataframe to tabular no header: length | PASS   |    1|  0.001|

</details>

<details> <summary> Session Info </summary>

| Field    | Value                               |
|:---------|:------------------------------------|
| Version  | R version 3.5.1 (2018-07-02)        |
| Platform | x86\_64-apple-darwin15.6.0 (64-bit) |
| Running  | macOS High Sierra 10.13.6           |
| Language | en\_US                              |
| Timezone | America/New\_York                   |

| Package  | Version    |
|:---------|:-----------|
| testthat | 2.0.0.9000 |
| covr     | 3.2.0      |
| covrpage | 0.0.66     |

</details>

<!--- Final Status : pass --->
