test_that("makeOxygen works", {
  df <- as.data.frame(LETTERS)
  # TODO: Modify test for compatibility w/ testthat 3rd edition
  # expect_snapshot(makeOxygen(df))
  # expect_snapshot(makeOxygen(makeOxygen))
  
  expect_type(makeOxygen(df), "character")
  expect_type(makeOxygen(makeOxygen), "character")
  
  skip_if_not(interactive())
  expect_message(makeOxygen(df), class = "cli")
  
  skip_if_not_installed("clipr")
  skip_if_not(interactive())
  expect_message(makeOxygen(df), "Copied to clipboard")
  expect_message(makeOxygen(makeOxygen), "Copied to clipboard")
  out <- makeOxygen(makeOxygen)
  clip_out <- paste0(clipr::read_clip(), collapse = "\n")
  expect_identical(out, clip_out)
})
