test_that("extract.symbols.from.ast works", {
  expect_equal(extract.symbols.from.ast(rlang::expr(is.na(y))), c("is.na", "y"))
  expect_equal(
    extract.symbols.from.ast(rlang::expr(stats::runif(n = 10, 1, 2))),
    c("::", "stats", "runif")
  )
  expect_equal(extract.symbols.from.ast("3"), character(0))
})

test_that("get.all.functions works", {
  expect_true(is.character(get.all.functions()))
})
