Tests and Coverage
================
13 August, 2018 09:39:56

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                             | Coverage (%) |
| :------------------------------------------------- | :----------: |
| sinew                                              |     2.42     |
| [R/check\_attach.R](../R/check_attach.R)           |     0.00     |
| [R/create\_yml.R](../R/create_yml.R)               |     0.00     |
| [R/defaults.R](../R/defaults.R)                    |     0.00     |
| [R/get\_oxy.R](../R/get_oxy.R)                     |     0.00     |
| [R/importAddin.R](../R/importAddin.R)              |     0.00     |
| [R/interOxyAddIn.R](../R/interOxyAddIn.R)          |     0.00     |
| [R/ls\_param.R](../R/ls_param.R)                   |     0.00     |
| [R/make\_import.R](../R/make_import.R)             |     0.00     |
| [R/make\_seealso.R](../R/make_seealso.R)           |     0.00     |
| [R/makeDictionary.R](../R/makeDictionary.R)        |     0.00     |
| [R/makeOxyFile.R](../R/makeOxyFile.R)              |     0.00     |
| [R/makeOxygen.R](../R/makeOxygen.R)                |     0.00     |
| [R/moga.R](../R/moga.R)                            |     0.00     |
| [R/oxygenAddin.R](../R/oxygenAddin.R)              |     0.00     |
| [R/pretty\_namespace.R](../R/pretty_namespace.R)   |     0.00     |
| [R/pretty\_utils.R](../R/pretty_utils.R)           |     0.00     |
| [R/rm\_oxylines.R](../R/rm_oxylines.R)             |     0.00     |
| [R/something.R](../R/something.R)                  |     0.00     |
| [R/tabular.R](../R/tabular.R)                      |     0.00     |
| [R/untangle\_examples.R](../R/untangle_examples.R) |     0.00     |
| [R/untangle.R](../R/untangle.R)                    |     0.00     |
| [R/zzz.R](../R/zzz.R)                              |    15.38     |
| [R/rmOxygen.R](../R/rmOxygen.R)                    |    95.45     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

|           | file                            | n |  time | error | failed | skipped | warning |
| --------- | :------------------------------ | -: | ----: | ----: | -----: | ------: | ------: |
| test-rm.R | [test-rm.R](testthat/test-rm.R) | 4 | 0.012 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results
</summary>

| file                                    | context       | test                           | status | n |  time |
| :-------------------------------------- | :------------ | :----------------------------- | :----- | -: | ----: |
| [test-rm.R](testthat/test-rm.R#L24_L27) | remove oxygen | rm valid actions: no show file | PASS   | 1 | 0.004 |
| [test-rm.R](testthat/test-rm.R#L34_L37) | remove oxygen | rm valid actions: show file    | PASS   | 1 | 0.004 |
| [test-rm.R](testthat/test-rm.R#L47_L50) | remove oxygen | rm invalid actions: extension  | PASS   | 1 | 0.002 |
| [test-rm.R](testthat/test-rm.R#L56_L59) | remove oxygen | rm invalid actions: path       | PASS   | 1 | 0.002 |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                               |
| :------- | :---------------------------------- |
| Version  | R version 3.5.0 (2018-04-23)        |
| Platform | x86\_64-apple-darwin15.6.0 (64-bit) |
| Running  | macOS High Sierra 10.13.5           |
| Language | en\_US                              |
| Timezone | America/New\_York                   |

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.0   |
| covr     | 3.1.0   |
| covrpage | 0.0.5   |

</details>

<!--- Final Status : pass --->
