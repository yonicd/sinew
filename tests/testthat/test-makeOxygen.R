test_that("makeOxygen works", {
  df <- as.data.frame(LETTERS)
  expect_snapshot(makeOxygen(df))
  expect_snapshot(makeOxygen(makeOxygen))
  
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
