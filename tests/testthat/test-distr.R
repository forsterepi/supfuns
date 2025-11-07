test_that("rdiscunif works", {
  expect_equal(rdiscunif(1e3, 1, 1), rep(1, 1e3))
  expect_error(rdiscunif(5, 2, 1))
  expect_true(all(rdiscunif(1e3, 1, 3) >= 1))
  expect_true(all(rdiscunif(1e3, 1, 3) <= 3))

  expect_error(rdiscunif(3, c(1, 2), c(2, 3)))
  expect_no_error(rdiscunif(3, c(1, 2, 3), c(2, 3, 4)))
})
