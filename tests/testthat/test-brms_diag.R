test_that("diagnostic functions work", {
  f1 <- readRDS(test_path("fixtures", "brmsfit1.rds"))
  f2 <- readRDS(test_path("fixtures", "brmsfit2.rds"))

  # brms_diag_tree
  expect_equal(brms_diag_tree(f1), 24)
  expect_equal(brms_diag_tree(f2), 0)

  # brms_diag_div
  expect_equal(brms_diag_div(f1), 0)
  expect_equal(brms_diag_div(f2), 433)

  # brms_diag_bfmi
  expect_equal(brms_diag_bfmi(f1), 0)
  expect_equal(brms_diag_bfmi(f2), 0)

  # brms_diag_rhat
  expect_equal(brms_diag_rhat(f1), 0)
  expect_equal(brms_diag_rhat(f2), 3)

  # brms_diag_ess_bulk
  expect_equal(brms_diag_ess_bulk(f1), 0)
  expect_equal(brms_diag_ess_bulk(f2), 3)

  # brms_diag_ess_tail
  expect_equal(brms_diag_ess_tail(f1), 0)
  expect_equal(brms_diag_ess_tail(f2), 3)

  # brms_diag
  expect_equal(
    brms_diag(f1),
    list(
      div = 0,
      tree = 24,
      bfmi = 0,
      rhat = 0,
      ess_bulk = 0,
      ess_tail = 0
    )
  )
  expect_equal(
    brms_diag(f2),
    list(
      div = 433,
      tree = 0,
      bfmi = 0,
      rhat = 3,
      ess_bulk = 3,
      ess_tail = 3
    )
  )
})
