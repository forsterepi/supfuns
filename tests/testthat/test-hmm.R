test_that("create_hmm_data works", {
  df <- create_hmm_data()
  expect_equal(
    colnames(df),
    c(
      "ID", "time", "asthma", "state", "obs1", "obs2", "age_pred",
      "female", "inb2", "vab1", "vab2"
    )
  )
  expect_true(is.factor(df$female))
  expect_true(is.factor(df$inb2))
  expect_true(is.factor(df$vab1))
  expect_true(is.factor(df$vab2))
  expect_true(df$obs1 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_true(df$obs2 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_true(df$vab1 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_true(df$vab2 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))

  df <- create_hmm_data(
    use_state = FALSE, use_time = FALSE, to_factor = FALSE,
    na_obs = FALSE, na_cov = FALSE
  )
  expect_equal(
    colnames(df),
    c(
      "ID", "asthma", "obs1", "obs2", "age_pred", "female", "inb2", "vab1",
      "vab2"
    )
  )
  expect_false(is.factor(df$female))
  expect_false(is.factor(df$inb2))
  expect_false(is.factor(df$vab1))
  expect_false(is.factor(df$vab2))
  expect_false(df$obs1 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_false(df$obs2 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_false(df$vab1 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
  expect_false(df$vab2 %>% is.na() %>% sum() %>% magrittr::is_greater_than(0))
})

test_that("n_trans works", {
  set.seed(370990)
  df <- create_hmm_data() %>%
    dplyr::filter(ID <= 5)
  x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2)

  v <- n_trans(x$hmm)
  expect_equal(v$rem, 4)
  expect_equal(v$rel, 3)

  by_id_v <- data.frame(
    ID = c("1", "2", "3", "4", "5"),
    remrel1 = c("01", "11", "21", "00", "10")
  )

  expect_equal(v$by_id, by_id_v)

  set.seed(92510)
  s <- n_trans(x$hmm, nsamp = 5, diag_mode = TRUE)
  expect_equal(s$rem, c(4, 7, 4, 4, 4))
  expect_equal(s$rel, c(3, 6, 3, 3, 3))

  by_id_s <- data.frame(
    ID = c("1", "2", "3", "4", "5"),
    remrel1 = c("01", "11", "21", "00", "10"),
    remrel2 = c("01", "22", "22", "21", "10"),
    remrel3 = c("01", "11", "21", "00", "10"),
    remrel4 = c("01", "11", "21", "00", "10"),
    remrel5 = c("01", "11", "21", "00", "10")
  )

  expect_equal(s$by_id, by_id_s)
})

test_that("or_hmm works", {
  set.seed(378581)
  df <- create_hmm_data()
  x0 <- run_hmm(df,
    obsvars = c("obs1", "obs2"), nrep = 2
  )$hmm

  expect_no_error(or_hmm(x0))

  x <- run_hmm(df,
    obsvars = c("obs1", "obs2"), nrep = 2,
    formula = ~ age_pred * female + vab1 * female
  )$hmm

  or1 <- data.frame(
    Transition = rep(c("Relapse", "Remission"), each = 3L),
    Parameter = rep(c("female1", "age_pred:female1", "female1:vab11"), 2),
    OR = c(
      "14.83 (1.14-192.99)", "0.88 (0.76-1.00)", "0.57 (0.19-1.66)",
      "1.42 (0.19-10.80)", "1.02 (0.91-1.14)", "0.50 (0.20-1.25)"
    )
  )

  or2 <- data.frame(
    Transition = rep(c("Relapse", "Remission"), each = 4L),
    Parameter = rep(c("female1", "vab11", "age_pred:female1", "female1:vab11"), 2),
    OR = c(
      "14.83 (1.14-192.99)", "1.60 (0.85-3.01)", "0.88 (0.76-1.00)",
      "0.57 (0.19-1.66)", "1.42 (0.19-10.80)", "1.48 (0.83-2.65)",
      "1.02 (0.91-1.14)", "0.50 (0.20-1.25)"
    )
  )

  or3 <- data.frame(
    Transition = rep(c("Relapse", "Remission"), each = 5L),
    Parameter = rep(c("age_pred", "female1", "vab11", "age_pred:female1", "female1:vab11"), 2),
    OR = c(
      "1.07 (0.99-1.16)", "14.83 (1.14-192.99)", "1.60 (0.85-3.01)",
      "0.88 (0.76-1.00)", "0.57 (0.19-1.66)", "1.00 (0.93-1.06)",
      "1.42 (0.19-10.80)", "1.48 (0.83-2.65)", "1.02 (0.91-1.14)",
      "0.50 (0.20-1.25)"
    )
  )

  expect_equal(or_hmm(x, var = "female"), or1)
  expect_equal(or_hmm(x, var = c("female", "vab1")), or2)
  expect_equal(or_hmm(x, var = c("female", "vab1", "age_pred")), or3)
})

test_that("create_df_plot_hmm_obs works", {
  set.seed(28768)
  df <- create_hmm_data()
  x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2)

  comp_obse <- data.frame(
    rowname = factor(rep(c("obs1", "obs2"), each = 2L),
      levels = c("obs2", "obs1")
    ),
    mle = c(
      0.06139850656289725, 0.3056596816960332, 0.1797679746014399,
      0.7128032576283053
    ),
    lcl = c(
      0.048641478501845414, 0.27813589541095723, 0.15842419136916794,
      0.6827738967650607
    ),
    ucl = c(
      0.07722967358295886, 0.3346445049252082, 0.2032926863036597,
      0.7410680023842589
    ),
    state = factor(rep(c("No Asthma", "Asthma"), 2),
      levels = c("No Asthma", "Asthma")
    )
  )

  comp_obse1 <- data.frame(
    rowname = factor(c("obs1", "obs2"), levels = c("obs2", "obs1")),
    mle = c(0.06139850656289725, 0.1797679746014399),
    lcl = c(0.048641478501845414, 0.15842419136916794),
    ucl = c(0.07722967358295886, 0.2032926863036597),
    state = factor("No Asthma", levels = c("No Asthma", "Asthma")),
    row.names = c(1L, 3L)
  )

  comp_obse2 <- data.frame(
    rowname = factor(c("obs1", "obs2"), levels = c("obs2", "obs1")),
    mle = c(0.3056596816960332, 0.7128032576283053),
    lcl = c(0.27813589541095723, 0.6827738967650607),
    ucl = c(0.3346445049252082, 0.7410680023842589),
    state = factor("Asthma", levels = c("No Asthma", "Asthma")),
    row.names = c(2L, 4L)
  )

  comp_lines <- 1.5

  out <- create_df_plot_hmm_obs(x$hmm)
  expect_equal(out$obse, comp_obse)
  expect_equal(out$obse1, comp_obse1)
  expect_equal(out$obse2, comp_obse2)
  expect_equal(out$lines, comp_lines)
})

test_that("plot_hmm_obs works", {
  set.seed(198788)
  df <- create_hmm_data()
  x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2)

  expect_no_error(plot_hmm_obs(x$hmm))
  p <- plot_hmm_obs(x$hmm)

  l1 <- data.frame(
    x = c(0.055702841486093446, 0.20915903797052568),
    y = c(2.2, 1.2),
    xmin = c(0.043597568288868876, 0.186775034343505),
    xmax = c(0.07092001869224442, 0.2334555410322395),
    colour = "#E41A1C",
    PANEL = factor("1"),
    group = 2:1 |>
      structure(n = 2L),
    flipped_aes = TRUE,
    nudge_x = 0,
    nudge_y = 0,
    size = 0.5,
    linewidth = 0.5,
    linetype = 1,
    shape = 19,
    fill = NA,
    alpha = NA,
    stroke = 1
  )

  l2 <- data.frame(
    x = c(0.30125743573509334, 0.6848175285837329),
    y = c(1.8, 0.8),
    xmin = c(0.2729992440198061, 0.6531368775596605),
    xmax = c(0.3311084576481318, 0.7148678356393614),
    colour = "#377EB8",
    PANEL = factor("1"),
    group = 2:1 |>
      structure(n = 2L),
    flipped_aes = TRUE,
    nudge_x = 0,
    nudge_y = 0,
    size = 0.5,
    linewidth = 0.5,
    linetype = 1,
    shape = 19,
    fill = NA,
    alpha = NA,
    stroke = 1
  )

  l3 <- data.frame(
    yintercept = 1.5,
    PANEL = 1,
    group = -1L |>
      structure(n = 1L),
    colour = "black",
    linewidth = 0.5,
    linetype = "dashed",
    alpha = NA
  )

  expect_equal(ggplot2::layer_data(p, 1) %>%
    dplyr::mutate("y" := as.numeric(.data$y)), l1)
  expect_equal(ggplot2::layer_data(p, 2) %>%
    dplyr::mutate("y" := as.numeric(.data$y)), l2)
  expect_equal(ggplot2::layer_data(p, 3), l3)
})

test_that("plot_hmm_predict works", {
  set.seed(285300)
  df <- create_hmm_data()
  x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2)$hmm

  expect_error(plot_hmm_predict(x), class = "plot_hmm_predict_03")

  set.seed(473333)
  x <- run_hmm(df,
    obsvars = c("obs1", "obs2"), nrep = 2,
    formula = ~ age_pred * female + vab1
  )$hmm
  p <- plot_hmm_predict(x, diag_mode = TRUE)
  expect_equal(p$age_sex, c("age_pred", "female"))
  expect_equal(p$var, "vab1")
  expect_equal(p$facet_vars, "female")

  set.seed(719438)
  x <- run_hmm(df,
    obsvars = c("obs1", "obs2"), nrep = 2,
    formula = ~ age_pred * female + vab1 + vab2 + inb2
  )$hmm

  expect_error(plot_hmm_predict(x, fix = list(inb2 = c("0", "1"))),
    class = "plot_hmm_predict_01"
  )
  expect_error(plot_hmm_predict(x, var = "y"), class = "plot_hmm_predict_02")
  expect_error(plot_hmm_predict(x), class = "plot_hmm_predict_03")
  expect_error(plot_hmm_predict(x, var = "vab1", facet = ~y),
    class = "plot_hmm_predict_04"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", fix = list(y = "1")),
    class = "plot_hmm_predict_05"
  )
  expect_error(plot_hmm_predict(x, var = "age_pred"),
    class = "plot_hmm_predict_06"
  )
  expect_error(plot_hmm_predict(x, var = "female"),
    class = "plot_hmm_predict_06"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", fix = list(age_pred = 1)),
    class = "plot_hmm_predict_07"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", fix = list(female = "1")),
    class = "plot_hmm_predict_07"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", facet = ~age_pred),
    class = "plot_hmm_predict_08"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", facet = ~vab1),
    class = "plot_hmm_predict_09"
  )
  expect_error(plot_hmm_predict(x, var = "vab1", fix = list(vab1 = "1")),
    class = "plot_hmm_predict_10"
  )
  expect_error(
    plot_hmm_predict(x,
      var = "vab1", facet = ~vab2,
      fix = list(vab2 = "1")
    ),
    class = "plot_hmm_predict_11"
  )
  expect_error(plot_hmm_predict(x, var = "vab1"), class = "plot_hmm_predict_12")

  expect_error(plot_hmm_predict(x, var = "vab1", facet = ~female),
    class = "plot_hmm_predict_13"
  )

  p <- plot_hmm_predict(x,
    var = "vab1", facet = female ~ vab2,
    fix = list(inb2 = "1"), diag_mode = TRUE
  )
  expect_equal(p$age_sex, c("age_pred", "female"))
  expect_equal(p$var, "vab1")
  expect_equal(p$facet_vars, c("female", "vab2"))
  expect_equal(p$fix_vars, "inb2")

  rel2 <- ggplot2::layer_data(p$rel, 2)
  rel3 <- ggplot2::layer_data(p$rel, 3)

  expect_equal(
    rel2 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y),
    rel3 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y)
  )

  par_rel <- x$coeff_fe()$hid %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(stringi::stri_detect_regex(rowname, "^S1>S2."))

  rel_f1_vab10_vab22 <- par_rel$V1[par_rel$rowname == "S1>S2.(Intercept)"] +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred"] * 0:12 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab11"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab22"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab23"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.inb22"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.inb23"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred:female1"] * 0:12 * 1
  rel_f1_vab10_vab22 %<>% plogis()

  expect_equal(
    rel3 %>%
      dplyr::filter(group == 1L, PANEL == "5") %>%
      extract2("y"),
    rel_f1_vab10_vab22
  )

  rem2 <- ggplot2::layer_data(p$rem, 2)
  rem3 <- ggplot2::layer_data(p$rem, 3)

  expect_equal(
    rem2 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y),
    rem3 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y)
  )

  par_rem <- x$coeff_fe()$hid %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(stringi::stri_detect_regex(rowname, "^S2>S1."))

  rem_f0_vab11_vab23 <- par_rem$V1[par_rem$rowname == "S2>S1.(Intercept)"] +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred"] * 0:12 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab11"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab22"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab23"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.inb22"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.inb23"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred:female1"] * 0:12 * 0
  rem_f0_vab11_vab23 %<>% plogis()

  expect_equal(
    rem3 %>%
      dplyr::filter(group == 2L, PANEL == "3") %>%
      extract2("y"),
    rem_f0_vab11_vab23
  )

  expect_no_error(p$rem)
  expect_no_error(p$rel)
})

test_that("plot_hmm_predict works with interactions with female", {
  set.seed(378581)
  df <- create_hmm_data()
  x <- run_hmm(df,
    obsvars = c("obs1", "obs2"), nrep = 2,
    formula = ~ age_pred * female + vab1 * female
  )$hmm
  p <- plot_hmm_predict(x, diag_mode = TRUE)

  # rem
  par_rem <- x$coeff_fe()$hid %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(stringi::stri_detect_regex(rowname, "^S2>S1."))

  rem_f0_vab10 <- par_rem$V1[par_rem$rowname == "S2>S1.(Intercept)"] +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred"] * 0:12 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab11"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred:female1"] * 0:12 * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1:vab11"] * 0 * 0
  rem_f0_vab10 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 0, vab1 == 0) %>%
      magrittr::extract2("rem_est"),
    rem_f0_vab10
  )

  rem_f1_vab10 <- par_rem$V1[par_rem$rowname == "S2>S1.(Intercept)"] +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred"] * 0:12 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab11"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred:female1"] * 0:12 * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1:vab11"] * 1 * 0
  rem_f1_vab10 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 1, vab1 == 0) %>%
      magrittr::extract2("rem_est"),
    rem_f1_vab10
  )

  rem_f0_vab11 <- par_rem$V1[par_rem$rowname == "S2>S1.(Intercept)"] +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred"] * 0:12 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1"] * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab11"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred:female1"] * 0:12 * 0 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1:vab11"] * 0 * 1
  rem_f0_vab11 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 0, vab1 == 1) %>%
      magrittr::extract2("rem_est"),
    rem_f0_vab11
  )

  rem_f1_vab11 <- par_rem$V1[par_rem$rowname == "S2>S1.(Intercept)"] +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred"] * 0:12 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.vab11"] * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.age_pred:female1"] * 0:12 * 1 +
    par_rem$V1[par_rem$rowname == "S2>S1.female1:vab11"] * 1 * 1
  rem_f1_vab11 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 1, vab1 == 1) %>%
      magrittr::extract2("rem_est"),
    rem_f1_vab11
  )

  rem2 <- ggplot2::layer_data(p$rem, 2)
  rem3 <- ggplot2::layer_data(p$rem, 3)

  expect_equal(
    rem2 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y),
    rem3 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y)
  )

  expect_equal(
    rem3 %>%
      dplyr::filter(group == 1L, PANEL == "1") %>%
      extract2("y"),
    rem_f0_vab10
  )

  expect_equal(
    rem3 %>%
      dplyr::filter(group == 1L, PANEL == "2") %>%
      extract2("y"),
    rem_f1_vab10
  )

  expect_equal(
    rem3 %>%
      dplyr::filter(group == 2L, PANEL == "1") %>%
      extract2("y"),
    rem_f0_vab11
  )

  expect_equal(
    rem3 %>%
      dplyr::filter(group == 2L, PANEL == "2") %>%
      extract2("y"),
    rem_f1_vab11
  )

  # rel
  par_rel <- x$coeff_fe()$hid %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(stringi::stri_detect_regex(rowname, "^S1>S2."))

  rel_f0_vab10 <- par_rel$V1[par_rel$rowname == "S1>S2.(Intercept)"] +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred"] * 0:12 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab11"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred:female1"] * 0:12 * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1:vab11"] * 0 * 0
  rel_f0_vab10 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 0, vab1 == 0) %>%
      magrittr::extract2("rel_est"),
    rel_f0_vab10
  )

  rel_f1_vab10 <- par_rel$V1[par_rel$rowname == "S1>S2.(Intercept)"] +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred"] * 0:12 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab11"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred:female1"] * 0:12 * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1:vab11"] * 1 * 0
  rel_f1_vab10 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 1, vab1 == 0) %>%
      magrittr::extract2("rel_est"),
    rel_f1_vab10
  )

  rel_f0_vab11 <- par_rel$V1[par_rel$rowname == "S1>S2.(Intercept)"] +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred"] * 0:12 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1"] * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab11"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred:female1"] * 0:12 * 0 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1:vab11"] * 0 * 1
  rel_f0_vab11 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 0, vab1 == 1) %>%
      magrittr::extract2("rel_est"),
    rel_f0_vab11
  )

  rel_f1_vab11 <- par_rel$V1[par_rel$rowname == "S1>S2.(Intercept)"] +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred"] * 0:12 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.vab11"] * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.age_pred:female1"] * 0:12 * 1 +
    par_rel$V1[par_rel$rowname == "S1>S2.female1:vab11"] * 1 * 1
  rel_f1_vab11 %<>% plogis()

  expect_equal(
    p$newdata %>%
      dplyr::filter(female == 1, vab1 == 1) %>%
      magrittr::extract2("rel_est"),
    rel_f1_vab11
  )

  rel2 <- ggplot2::layer_data(p$rel, 2)
  rel3 <- ggplot2::layer_data(p$rel, 3)

  expect_equal(
    rel2 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y),
    rel3 %>% dplyr::select(x, y) %>% dplyr::arrange(x, y)
  )

  expect_equal(
    rel3 %>%
      dplyr::filter(group == 1L, PANEL == "1") %>%
      extract2("y"),
    rel_f0_vab10
  )

  expect_equal(
    rel3 %>%
      dplyr::filter(group == 1L, PANEL == "2") %>%
      extract2("y"),
    rel_f1_vab10
  )

  expect_equal(
    rel3 %>%
      dplyr::filter(group == 2L, PANEL == "1") %>%
      extract2("y"),
    rel_f0_vab11
  )

  expect_equal(
    rel3 %>%
      dplyr::filter(group == 2L, PANEL == "2") %>%
      extract2("y"),
    rel_f1_vab11
  )
})

test_that("run_hmm works", {
  df <- create_hmm_data()
  obsvars <- c("obs1", "obs2")

  # Without dignostic mode
  expect_no_error(run_hmm(df, obsvars, nrep = 2))
  expect_no_error(run_hmm(df, obsvars, nrep = 1))
  expect_no_error(run_hmm(df, obsvars, nrep = 2, initial_state = "shared"))
  expect_no_error(run_hmm(df, obsvars, nrep = 1, initial_state = "shared"))
  expect_no_error(run_hmm(df, obsvars, nrep = 2, initial_state = "stationary"))
  expect_no_error(run_hmm(df, obsvars, nrep = 1, initial_state = "stationary"))
  expect_no_error(run_hmm(df, obsvars, nrep = 2, formula = ~vab1))
  expect_no_error(run_hmm(df, obsvars, nrep = 1, formula = ~vab1))

  # With diagnostic mode
  # default initial_state, no formula, nrep = 2
  x <- run_hmm(df, obsvars, nrep = 2, diag_mode = TRUE)
  checkmate::expect_r6(x$hmm, classes = "HMM")
  expect_lte(x$ident, 1)
  expect_gte(x$ident, 0)
  checkmate::expect_r6(x$diag_all_models[[1]], classes = "HMM")
  checkmate::expect_r6(x$diag_all_models[[2]], classes = "HMM")
  expect_equal(
    x$diag_hid_draws[[1]],
    x$diag_hid_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_hid_draws[[2]],
    x$diag_hid_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_hid_draws[[1]], x$diag_hid_draws[[2]]))
  expect_false(identical(x$diag_hid_updated[[1]], x$diag_hid_updated[[2]]))
  expect_equal(
    x$diag_obs_draws[[1]] %>% qlogis(),
    x$diag_obs_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws[[2]] %>% qlogis(),
    x$diag_obs_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_obs_draws[[1]], x$diag_obs_draws[[2]]))
  expect_false(identical(x$diag_obs_updated[[1]], x$diag_obs_updated[[2]]))
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(0.1, 0.1) %>% qlogis()
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  # default initial_state, no formula, nrep = 1
  x <- run_hmm(df, obsvars, nrep = 1, diag_mode = TRUE)
  checkmate::expect_r6(x$hmm, classes = "HMM")
  checkmate::expect_scalar_na(x$ident)
  expect_null(x$diag_all_models)
  expect_equal(
    x$diag_hid_draws,
    x$diag_hid_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws %>% qlogis(),
    x$diag_obs_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(0.1, 0.1) %>% qlogis()
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  # "shared" is default initial_state, nrep = 2
  set.seed(487587)
  x1 <- run_hmm(df, obsvars,
    nrep = 2, initial_state = "shared",
    diag_mode = TRUE
  )
  set.seed(487587)
  x2 <- run_hmm(df, obsvars, nrep = 2, diag_mode = TRUE)
  expect_identical(x1, x2)

  # "shared" is default initial_state, nrep = 1
  set.seed(140372)
  x1 <- run_hmm(df, obsvars,
    nrep = 1, initial_state = "shared",
    diag_mode = TRUE
  )
  set.seed(140372)
  x2 <- run_hmm(df, obsvars, nrep = 1, diag_mode = TRUE)
  expect_identical(x1, x2)

  # initial_state "stationary", no formula, nrep = 2
  x <- run_hmm(df, obsvars,
    nrep = 2, initial_state = "stationary",
    diag_mode = TRUE
  )
  checkmate::expect_r6(x$hmm, classes = "HMM")
  expect_lte(x$ident, 1)
  expect_gte(x$ident, 0)
  checkmate::expect_r6(x$diag_all_models[[1]], classes = "HMM")
  checkmate::expect_r6(x$diag_all_models[[2]], classes = "HMM")
  expect_equal(
    x$diag_hid_draws[[1]],
    x$diag_hid_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_hid_draws[[2]],
    x$diag_hid_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_hid_draws[[1]], x$diag_hid_draws[[2]]))
  expect_false(identical(x$diag_hid_updated[[1]], x$diag_hid_updated[[2]]))
  expect_equal(
    x$diag_obs_draws[[1]] %>% qlogis(),
    x$diag_obs_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws[[2]] %>% qlogis(),
    x$diag_obs_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_obs_draws[[1]], x$diag_obs_draws[[2]]))
  expect_false(identical(x$diag_obs_updated[[1]], x$diag_obs_updated[[2]]))
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(0.1, 0.1) %>% qlogis()
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  # initial_state "stationary", no formula, nrep = 1
  x <- run_hmm(df, obsvars,
    nrep = 1, initial_state = "stationary",
    diag_mode = TRUE
  )
  checkmate::expect_r6(x$hmm, classes = "HMM")
  checkmate::expect_scalar_na(x$ident)
  expect_null(x$diag_all_models)
  expect_equal(
    x$diag_hid_draws,
    x$diag_hid_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws %>% qlogis(),
    x$diag_obs_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(0.1, 0.1) %>% qlogis()
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  ## with formula, nrep = 2
  x <- run_hmm(df, obsvars, nrep = 2, formula = ~vab1, diag_mode = TRUE)
  checkmate::expect_r6(x$hmm, classes = "HMM")
  expect_lte(x$ident, 1)
  expect_gte(x$ident, 0)
  checkmate::expect_r6(x$diag_all_models[[1]], classes = "HMM")
  checkmate::expect_r6(x$diag_all_models[[2]], classes = "HMM")
  expect_equal(
    x$diag_hid_draws[[1]],
    x$diag_hid_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_hid_draws[[2]],
    x$diag_hid_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_hid_draws[[1]], x$diag_hid_draws[[2]]))
  expect_false(identical(x$diag_hid_updated[[1]], x$diag_hid_updated[[2]]))
  expect_equal(
    x$diag_obs_draws[[1]] %>% qlogis(),
    x$diag_obs_updated[[1]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws[[2]] %>% qlogis(),
    x$diag_obs_updated[[2]][, 1] %>% magrittr::set_names(NULL)
  )
  expect_false(identical(x$diag_obs_draws[[1]], x$diag_obs_draws[[2]]))
  expect_false(identical(x$diag_obs_updated[[1]], x$diag_obs_updated[[2]]))
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(qlogis(0.1), 0, qlogis(0.1), 0)
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  ## with formula, nrep = 1
  x <- run_hmm(df, obsvars, nrep = 1, formula = ~vab1, diag_mode = TRUE)
  checkmate::expect_r6(x$hmm, classes = "HMM")
  checkmate::expect_scalar_na(x$ident)
  expect_null(x$diag_all_models)
  expect_equal(
    x$diag_hid_draws,
    x$diag_hid_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_obs_draws %>% qlogis(),
    x$diag_obs_updated[, 1] %>% magrittr::set_names(NULL)
  )
  expect_equal(
    x$diag_unfitted_hid[, 1] %>% magrittr::set_names(NULL),
    c(qlogis(0.1), 0, qlogis(0.1), 0)
  )
  expect_equal(
    x$diag_unfitted_obs[, 1] %>% magrittr::set_names(NULL),
    rep(0, 4)
  )

  # Compare diag_mode = FALSE with diag_mode = TRUE
  set.seed(590522)
  x1 <- run_hmm(df, obsvars, nrep = 2, diag_mode = TRUE)
  set.seed(590522)
  x2 <- run_hmm(df, obsvars, nrep = 2, diag_mode = FALSE)
  expect_identical(x1[1:2], x2)

  set.seed(470510)
  x1 <- run_hmm(df, obsvars, nrep = 1, diag_mode = TRUE)
  set.seed(470510)
  x2 <- run_hmm(df, obsvars, nrep = 1, diag_mode = FALSE)
  expect_identical(x1[1:2], x2)

  set.seed(565222)
  x1 <- run_hmm(df, obsvars, nrep = 2, formula = ~vab1, diag_mode = TRUE)
  set.seed(565222)
  x2 <- run_hmm(df, obsvars, nrep = 2, formula = ~vab1, diag_mode = FALSE)
  expect_identical(x1[1:2], x2)

  set.seed(653396)
  x1 <- run_hmm(df, obsvars, nrep = 1, formula = ~vab1, diag_mode = TRUE)
  set.seed(653396)
  x2 <- run_hmm(df, obsvars, nrep = 1, formula = ~vab1, diag_mode = FALSE)
  expect_identical(x1[1:2], x2)
})

test_that("best_hmm works", {
  df <- create_hmm_data()

  hmm_a <- run_hmm(df, obsvars = "obs1", nrep = 1)$hmm
  hmm_b <- run_hmm(df, obsvars = "obs2", nrep = 1)$hmm

  expect_false(identical(hmm_a$llk(), hmm_b$llk()))

  if (hmm_a$llk() < hmm_b$llk()) {
    expect_true(identical(best_hmm(hmm_a, hmm_b), hmm_a))
  }
  if (hmm_a$llk() > hmm_b$llk()) {
    expect_true(identical(best_hmm(hmm_a, hmm_b), hmm_b))
  }
})
