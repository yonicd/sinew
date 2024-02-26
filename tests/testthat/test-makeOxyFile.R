test_that("makeOxyFile works", {
  expect_error(makeOxyFile(tempfile("file.txt")))

  test_file <- system.file('untangle_test.R',package = 'sinew')

  # TODO: Modify test for compatibility w/ testthat 3rd edition
  # expect_snapshot(makeOxyFile(test_file))
  expect_type(makeOxyFile(test_file), "character")
  
  oxy_title_file <- makeOxyFile(test_file, dir.out = tempdir())
  expect_true(file.exists(oxy_title_file))
  
})
