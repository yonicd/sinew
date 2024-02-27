Tests and Coverage
================
27 February, 2024 14:36:25

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
| :---------------------------------------------------- | :----------: |
| sinew                                                 |    31.20     |
| [R/check\_attach.R](../R/check_attach.R)              |     0.00     |
| [R/create\_yml.R](../R/create_yml.R)                  |     0.00     |
| [R/get\_oxy.R](../R/get_oxy.R)                        |     0.00     |
| [R/interOxyAddIn.R](../R/interOxyAddIn.R)             |     0.00     |
| [R/ls\_param.R](../R/ls_param.R)                      |     0.00     |
| [R/makeDictionary.R](../R/makeDictionary.R)           |     0.00     |
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
| [R/opts.R](../R/opts.R)                               |     5.88     |
| [R/zzz.R](../R/zzz.R)                                 |    11.11     |
| [R/pretty\_namespace.R](../R/pretty_namespace.R)      |    43.48     |
| [R/pretty\_utils.R](../R/pretty_utils.R)              |    44.24     |
| [R/makeOxygen.R](../R/makeOxygen.R)                   |    52.63     |
| [R/makeImport.R](../R/makeImport.R)                   |    60.66     |
| [R/makeOxyFile.R](../R/makeOxyFile.R)                 |    76.19     |
| [R/tabular.R](../R/tabular.R)                         |    88.89     |
| [R/make\_seealso.R](../R/make_seealso.R)              |    94.12     |
| [R/prettify.R](../R/prettify.R)                       |    94.83     |
| [R/untangle.R](../R/untangle.R)                       |    96.67     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                              |  n |  time | error | failed | skipped | warning | icon |
| :------------------------------------------------ | -: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test-makeOxyFile.R](testthat/test-makeOxyFile.R) |  3 | 0.056 |     0 |      0 |       0 |       0 |      |
| [test-makeOxygen.R](testthat/test-makeOxygen.R)   |  4 | 0.095 |     0 |      0 |       1 |       0 | 🔶    |
| [test-pretty.R](testthat/test-pretty.R)           | 14 | 0.175 |     0 |      0 |       1 |       0 | 🔶    |
| [test-seealso.R](testthat/test-seealso.R)         |  3 | 0.010 |     0 |      0 |       0 |       0 |      |
| [test-tabular.R](testthat/test-tabular.R)         |  3 | 0.008 |     0 |      0 |       0 |       0 |      |
| [test-untangle.R](testthat/test-untangle.R)       |  6 | 0.020 |     0 |      0 |       0 |       0 |      |

<details open>

<summary> Show Detailed Test Results </summary>

| file                                                 | context      | test                                           | status  | n |  time | icon |
| :--------------------------------------------------- | :----------- | :--------------------------------------------- | :------ | -: | ----: | :--- |
| [test-makeOxyFile.R](testthat/test-makeOxyFile.R#L2) | makeOxyFile  | makeOxyFile works                              | PASS    | 3 | 0.056 |      |
| [test-makeOxygen.R](testthat/test-makeOxygen.R#L7)   | makeOxygen   | makeOxygen works                               | SKIPPED | 4 | 0.095 | 🔶    |
| [test-pretty.R](testthat/test-pretty.R#L9)           | pretty       | switches: force                                | SKIPPED | 1 | 0.003 | 🔶    |
| [test-pretty.R](testthat/test-pretty.R#L26)          | pretty       | switches: ignore                               | PASS    | 1 | 0.085 |      |
| [test-pretty.R](testthat/test-pretty.R#L33)          | pretty       | setup files: length                            | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L34)          | pretty       | setup files: class                             | PASS    | 1 | 0.004 |      |
| [test-pretty.R](testthat/test-pretty.R#L35)          | pretty       | setup files: names                             | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L40)          | pretty       | setup file: length                             | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L41)          | pretty       | setup file: class                              | PASS    | 1 | 0.003 |      |
| [test-pretty.R](testthat/test-pretty.R#L42)          | pretty       | setup file: names                              | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L47)          | pretty       | setup text: length                             | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L48)          | pretty       | setup text: class                              | PASS    | 1 | 0.006 |      |
| [test-pretty.R](testthat/test-pretty.R#L49)          | pretty       | setup text: names                              | PASS    | 1 | 0.002 |      |
| [test-pretty.R](testthat/test-pretty.R#L69)          | pretty       | full text: txt                                 | PASS    | 2 | 0.024 |      |
| [test-pretty.R](testthat/test-pretty.R#L80_L82)      | pretty       | full file: file                                | PASS    | 1 | 0.038 |      |
| [test-seealso.R](testthat/test-seealso.R#L7)         | make seealso | cutoff: less than cutoff                       | PASS    | 1 | 0.003 |      |
| [test-seealso.R](testthat/test-seealso.R#L16)        | make seealso | no cutoff: no elements                         | PASS    | 1 | 0.003 |      |
| [test-seealso.R](testthat/test-seealso.R#L22)        | make seealso | no cutoff: simple call                         | PASS    | 1 | 0.004 |      |
| [test-tabular.R](testthat/test-tabular.R#L7)         | tabular      | convert dataframe to tabular header: length    | PASS    | 1 | 0.002 |      |
| [test-tabular.R](testthat/test-tabular.R#L8)         | tabular      | convert dataframe to tabular header: class     | PASS    | 1 | 0.003 |      |
| [test-tabular.R](testthat/test-tabular.R#L16)        | tabular      | convert dataframe to tabular no header: length | PASS    | 1 | 0.003 |      |
| [test-untangle.R](testthat/test-untangle.R#L42)      | untangle     | parent functions: find the parents             | PASS    | 1 | 0.006 |      |
| [test-untangle.R](testthat/test-untangle.R#L46)      | untangle     | parent functions: parent indicies              | PASS    | 1 | 0.003 |      |
| [test-untangle.R](testthat/test-untangle.R#L53)      | untangle     | untangle inputs: empty                         | PASS    | 1 | 0.005 |      |
| [test-untangle.R](testthat/test-untangle.R#L58_L59)  | untangle     | untangle inputs: text functions                | PASS    | 1 | 0.002 |      |
| [test-untangle.R](testthat/test-untangle.R#L63_L64)  | untangle     | untangle inputs: text body                     | PASS    | 1 | 0.002 |      |
| [test-untangle.R](testthat/test-untangle.R#L69_L72)  | untangle     | untangle inputs: file                          | PASS    | 1 | 0.002 |      |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| 🛑      | ⚠️      | 🔶       |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                         |                                                                                                                                                                                                                                                               |
| :------- | :---------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Version  | R version 4.3.2 (2023-10-31)  |                                                                                                                                                                                                                                                               |
| Platform | x86\_64-pc-linux-gnu (64-bit) | <a href="https://github.com/yonicd/sinew/commit/a8a57801862e4d21010acd8e38b7a49022b79d5b/checks" target="_blank"><span title="Built on Github Actions">![](https://github.com/metrumresearchgroup/covrpage/blob/actions/inst/logo/gh.png?raw=true)</span></a> |
| Running  | Ubuntu 22.04.4 LTS            |                                                                                                                                                                                                                                                               |
| Language | C                             |                                                                                                                                                                                                                                                               |
| Timezone | UTC                           |                                                                                                                                                                                                                                                               |

| Package  | Version |
| :------- | :------ |
| testthat | 3.2.1   |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>

<!--- Final Status : skipped/warning --->
