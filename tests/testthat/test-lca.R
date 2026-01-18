test_that("lca functions work", {
  #lca_data <- readRDS(test_path("fixtures", "lca_data.rds"))
  lca_data <- lca_example_data
  f <- cbind(var1, var2, var3, var4) ~ 1
  lca2 <- poLCA::poLCA(f, lca_data, nclass = 2, nrep = 100, verbose = FALSE)
  lca3 <- poLCA::poLCA(f, lca_data, nclass = 3, nrep = 100, verbose = FALSE)

  expect_no_error(lca_ci(lca3, "var1", 1, 1L))
  expect_no_error(lca_ci_p(lca3, 1L))
  expect_no_error(lca_print(lca3))
  expect_no_error(lca_ident(lca3))
  expect_no_error(lca_extract(lca3))
  expect_no_error(lca_plot_input(lca3))
  expect_no_error(lca_table(list(lca2, lca3)))
})
