test_that("makeOxyFile works", {
  expect_error(makeOxyFile(tempfile("file.txt")))

  test_file <- system.file('untangle_test.R',package = 'sinew')

  expect_snapshot(makeOxyFile(test_file))

})
