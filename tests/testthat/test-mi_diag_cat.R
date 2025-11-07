test_that("mi_diag_cat works", {
  set.seed(601987)
  df <- data.frame(
    x = stats::rnorm(100, 0, 1),
    y = sample(c(1, 2, 3), size = 100, replace = TRUE, prob = c(0.2, 0.3, 0.5)),
    r = stats::runif(100, 0, 1)
  ) %>%
    dplyr::mutate(
      "y" := dplyr::case_when(
        .data$r < 0.05 ~ NA,
        .default = .data$y
      ),
      "r" := NULL,
      "y" := factor(.data$y)
    )
  impu <- mice::mice(df, m = 5, maxit = 5, seed = 34752)

  expect_no_error(p <- mi_diag_cat(imp = impu, var = "y"))

  l <- ggplot2::layer_data(p)

  denom <- df$y %>%
    is.na() %>%
    magrittr::not() %>%
    sum()

  prop <- summary(df$y)[1:3] / denom
  prop %<>% magrittr::set_names(NULL)

  expect_equal((l$ymax[l$group == 1] - l$ymin[l$group == 1]), prop[1])
  expect_equal((l$ymax[l$group == 2] - l$ymin[l$group == 2]), prop[2])
  expect_equal((l$ymax[l$group == 3] - l$ymin[l$group == 3]), prop[3])

  impu1 <- impu %>% mice::complete(1)
  prop1 <- summary(impu1$y) / nrow(impu1)
  prop1 %<>% magrittr::set_names(NULL)

  expect_equal((l$ymax[l$group == 4] - l$ymin[l$group == 4]), prop1[1])
  expect_equal((l$ymax[l$group == 5] - l$ymin[l$group == 5]), prop1[2])
  expect_equal((l$ymax[l$group == 6] - l$ymin[l$group == 6]), prop1[3])
})
