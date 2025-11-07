test_that("seed.pls works", {
  expect_no_error(seed.pls())
  expect_equal(length(seed.pls(4, print = FALSE)), 4L)
  expect_true(is.integer(seed.pls(4, print = FALSE)))
})
