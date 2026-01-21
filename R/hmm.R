#' Create simple data for testing
#'
#' @param use_state TRUE (default) or FALSE, indicating if a `state` variable is
#' supposed to be included for semi-supervised HMMs
#' @param use_time TRUE (default) or FALSE, indicating if a `time` variable is
#' supposed to be included
#' @param to_factor TRUE (default) or FALSE, indicating if the categorical
#' covariates should be transormed to factor
#' @param na_obs TRUE (default) or FALSE, indicating if the variables caused by
#' the latent states include NAs
#' @param na_cov TRUE (default) or FALSE, indicating if the covariats include
#' NAs
#'
#' @returns A data.frame that can be used in `run_hmm()`
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' str(df)
create_hmm_data <- function(use_state = TRUE, use_time = TRUE,
                             to_factor = TRUE, na_obs = TRUE, na_cov = TRUE) {
  # Check inputs
  checkmate::assert_flag(use_state, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(use_time, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(to_factor, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(na_obs, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(na_cov, na.ok = FALSE, null.ok = FALSE)

  # Hard code parameters
  n_ids <- 200
  p_asthma_t1 <- 0.65
  p_remission <- 0.2
  p_relapse <- 0.1
  p_state <- 0.5
  p_obs1_1 <- 0.05
  p_obs1_2 <- 0.3
  p_obs2_1 <- 0.2
  p_obs2_2 <- 0.7
  p_female <- 0.3
  p_inb2_2 <- 0.3
  p_inb2_3 <- 0.3
  p_vab1 <- 0.7
  p_vab2_2 <- 0.2
  p_vab2_3 <- 0.6
  p_na <- 0.05

  # Create ID and time
  time <- rep(12:24, n_ids)
  ID <- rep(seq(1:n_ids), 13) %>% sort()
  df <- data.frame(ID = ID, time = time, asthma = rep(NA_real_, n_ids * 13))

  # Create hidden state sequence
  out <- vector(mode = "list", length = n_ids)
  for (i in seq_along(out)) {
    temp <- df %>% dplyr::filter(.data$ID == i)

    for (j in seq_len(nrow(temp))) {
      if (j == 1) {
        temp$asthma[j] <- sample(c(1, 2),
          size = 1,
          prob = c(1 - p_asthma_t1, p_asthma_t1)
        )
      } else {
        if (temp$asthma[j - 1] == 1) {
          temp$asthma[j] <- sample(c(1, 2),
            size = 1,
            prob = c(1 - p_relapse, p_relapse)
          )
        } else {
          temp$asthma[j] <- sample(c(1, 2),
            size = 1,
            prob = c(p_remission, 1 - p_remission)
          )
        }
      }
    }

    out[[i]] <- temp
  }

  df <- out %>% purrr::reduce(rbind)

  # Create state
  df$state <- df$asthma
  df$state_r <- stats::runif(n = nrow(df))
  df %<>%
    dplyr::mutate(
      "state" := dplyr::case_when(
        .data$state_r > p_state ~ NA,
        .default = .data$state
      ),
      "state_r" := NULL
    )

  # Create observed variables
  df$obs1 <- NA
  df$obs2 <- NA

  for (i in seq_len(nrow(df))) {
    if (df$asthma[i] == 1) {
      df$obs1[i] <- sample(c(1, 2), size = 1, prob = c(1 - p_obs1_1, p_obs1_1))
      df$obs2[i] <- sample(c(1, 2), size = 1, prob = c(1 - p_obs2_1, p_obs2_1))
    } else if (df$asthma[i] == 2) {
      df$obs1[i] <- sample(c(1, 2), size = 1, prob = c(1 - p_obs1_2, p_obs1_2))
      df$obs2[i] <- sample(c(1, 2), size = 1, prob = c(1 - p_obs2_2, p_obs2_2))
    }
  }

  # Create age_pred
  df$age_pred_help <- stats::runif(n = nrow(df), -0.49, 0.49) %>% round(2)
  df$age_pred <- df$time + df$age_pred_help
  df$age_pred_help <- NULL

  # Create invariant background variables
  df$female <- NA
  df$inb2 <- NA

  out <- vector(mode = "list", length = n_ids)
  for (i in seq_along(out)) {
    temp <- df %>% dplyr::filter(.data$ID == i)

    temp$female <- sample(c(0, 1), size = 1, prob = c(1 - p_female, p_female))
    temp$inb2 <- sample(c(1, 2, 3),
      size = 1,
      prob = c(1 - (p_inb2_2 + p_inb2_3), p_inb2_2, p_inb2_3)
    )

    out[[i]] <- temp
  }

  df <- out %>% purrr::reduce(rbind)

  # Create time-variant background
  df$vab1 <- sample(c(0, 1),
    size = nrow(df), replace = TRUE,
    prob = c(1 - p_vab1, p_vab1)
  )
  df$vab2 <- sample(c(1, 2, 3),
    size = nrow(df), replace = TRUE,
    prob = c(1 - (p_vab2_2 + p_vab2_3), p_vab2_2, p_vab2_3)
  )

  # Delete based on specified arguments
  if (!use_state) {
    df$state <- NULL
  }
  if (!use_time) {
    df$time <- NULL
  }

  # Intruduce missings to observed variables
  if (na_obs) {
    df$obs1_r <- stats::runif(n = nrow(df))
    df$obs2_r <- stats::runif(n = nrow(df))
    df %<>%
      dplyr::mutate(
        "obs1" := dplyr::case_when(
          .data$obs1_r < p_na ~ NA,
          .default = .data$obs1
        ),
        "obs1_r" := NULL,
        "obs2" := dplyr::case_when(
          .data$obs2_r < p_na ~ NA,
          .default = .data$obs2
        ),
        "obs2_r" := NULL
      )
  }

  # Intruduce missings to covariates
  if (na_cov) {
    df$vab1_r <- stats::runif(n = nrow(df))
    df$vab2_r <- stats::runif(n = nrow(df))
    df %<>%
      dplyr::mutate(
        "vab1" := dplyr::case_when(
          .data$vab1_r < p_na ~ NA,
          .default = .data$vab1
        ),
        "vab1_r" := NULL,
        "vab2" := dplyr::case_when(
          .data$vab2_r < p_na ~ NA,
          .default = .data$vab2
        ),
        "vab2_r" := NULL
      )
  }

  # Turn to factor if specified
  if (to_factor) {
    df %<>%
      dplyr::mutate(
        "female" := factor(.data$female),
        "inb2" := factor(.data$inb2),
        "vab1" := factor(.data$vab1),
        "vab2" := factor(.data$vab2)
      )
  }

  # Return
  df
}

#' Derive number of transitions
#'
#' @param hmm A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#' @param nsamp An integerish number indicating the number of state sequences
#' to be generated. If `nsamp` == 1, HMM$viterbi() is used. Otherwise,
#' HMM$sample_states() is used.
#' @param diag_mode TRUE or FALSE (default) indicating if diagnostic mode is
#' activated
#'
#' @returns A list with elements `rem`, `rel`, and `by_id`. `rem` and `rel` are
#' numeric vectors with length equal to `nsamp` and contain the number of
#' remission and relapse events, respectively. `by_id` is a data.frame with
#' `nsamp + 1` columns. The first column is called `ID` and contains participant
#' IDs, which are the unique values of variable `ID` in the data passed to the
#' `hmmTMB` objects. There is one row for each unique value in `ID`. The
#' remaining columns have name `remrel<number>` with `<number>` being replaced
#' by every value from 1 to `nsamp`, e.g., `remrel1` for the first of them.
#' They are of type character and contain a value that is a combination of two
#' digits: the first one represents the number of remission events, while the
#' second represents the number of relapse events. Remember the order of
#' remission first, relapse second with the variable name `remrel`. For example,
#' the value `21` means that the individual had 2 remission events and 1
#' relapse event.
#'
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 1)
#' n_trans(x$hmm)
n_trans <- function(hmm, nsamp = 1, diag_mode = FALSE) {
  # Check input
  checkmate::assert_r6(hmm, "HMM", null.ok = FALSE)
  checkmate::assert_integerish(nsamp,
    lower = 1, any.missing = FALSE,
    all.missing = FALSE, len = 1, null.ok = FALSE
  )
  checkmate::assert_flag(diag_mode, na.ok = FALSE, null.ok = FALSE)

  rlang::try_fetch(hmm$out(),
    error = function(cnd) {
      cli::cli_abort(c("Error!", cnd$message))
    }
  )

  # Extract data
  df <- hmm$obs()$data()

  rlang::try_fetch(checkmate::assert_subset(c("ID", "time"), colnames(df)),
    error = function(cnd) {
      cli::cli_abort("Dataset needs variables `ID` and `time`!")
    }
  )

  # Prepare data
  df %<>% dplyr::select("ID", "time")

  if (nsamp == 1) {
    df %<>% cbind(hmm$viterbi() %>%
      as.data.frame() %>%
      magrittr::set_colnames("s1"))
  } else {
    df %<>% cbind(hmm$sample_states(nsamp = nsamp) %>%
      as.data.frame() %>%
      magrittr::set_colnames(paste0("s", 1:nsamp)))
  }
  dfs <- df %>%
    dplyr::select(tidyselect::all_of(c("ID", paste0("s", 1:nsamp))))

  # Derive number of transitions
  out_rem <- rep(NA_real_, nsamp)
  out_rel <- rep(NA_real_, nsamp)
  uids <- df$ID %>% levels()
  out_id <- data.frame(ID = uids) %>%
    cbind(matrix(rep(NA_real_, length(uids) * nsamp),
      nrow = length(uids), ncol = nsamp
    ) %>%
      as.data.frame() %>%
      magrittr::set_colnames(paste0("remrel", 1:nsamp)))

  for (i in 1:nsamp) {
    temp <- df %>%
      dplyr::select(tidyselect::all_of(c("ID", "time", paste0("s", i)))) %>%
      magrittr::set_colnames(c("ID", "time", "s"))

    temp_rem <- 0
    temp_rel <- 0

    for (j in seq_along(uids)) {
      temp_rem_before <- temp_rem
      temp_rel_before <- temp_rel

      temp_id <- temp %>%
        dplyr::filter(.data$ID == uids[j]) %>%
        dplyr::arrange(.data$time)

      for (k in 2:nrow(temp_id)) {
        if (!identical(temp_id$s[k], temp_id$s[k - 1])) {
          if (temp_id$s[k - 1] == 2 && temp_id$s[k] == 1) {
            temp_rem <- temp_rem + 1
          }
          if (temp_id$s[k - 1] == 1 && temp_id$s[k] == 2) {
            temp_rel <- temp_rel + 1
          }
        }
      }

      out_id[out_id$ID == uids[j], paste0("remrel", i)] <- paste0(
        temp_rem - temp_rem_before, temp_rel - temp_rel_before
      )
    }

    out_rem[i] <- temp_rem
    out_rel[i] <- temp_rel
  }

  if (diag_mode) {
    list(rem = out_rem, rel = out_rel, by_id = out_id, samples = dfs)
  } else {
    list(rem = out_rem, rel = out_rel, by_id = out_id)
  }
}

#' Extract estimates for transition probability parameters
#'
#' @param hmm A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#' @param var A character vector containing the list of variables of interest.
#' All parameters that contain at least one of the strings in `var` are included
#' in the output table.
#'
#' @returns A data.frame with estimates for transition parameters
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2, formula = ~female)
#' or_hmm(x$hmm, var = "female")
or_hmm <- function(hmm, var = NULL) {
  # Check input
  checkmate::assert_r6(hmm, "HMM", null.ok = FALSE)
  rlang::try_fetch(hmm$out(),
    error = function(cnd) {
      cli::cli_abort(c("Error!", cnd$message))
    }
  )
  checkmate::assert_character(var,
    min.chars = 1, min.len = 1,
    any.missing = FALSE, null.ok = TRUE
  )

  # Prepare table
  out <- hmm$confint()$coeff_fe$hid %>%
    as.data.frame() %>%
    dplyr::select("mle", "lcl", "ucl") %>%
    tibble::rownames_to_column()

  if (!is.null(var)) {
    out %<>% dplyr::filter(var %>%
      purrr::map(\(x) stringi::stri_detect_fixed(.data$rowname, x)) %>%
      as.data.frame() %>%
      magrittr::set_colnames(var) %>%
      t() %>%
      as.data.frame() %>%
      purrr::map_lgl(any))
  }

  if (nrow(out) == 0) {
    cli::cli_abort("No parameters contain any variable name provided in `var`!")
  }

  out %<>%
    dplyr::mutate(
      "Transition" := dplyr::case_when(
        stringi::stri_detect_regex(.data$rowname, "^S1>S2") ~ "Relapse",
        stringi::stri_detect_regex(.data$rowname, "^S2>S1") ~ "Remission"
      ),
      "Parameter" := stringi::stri_replace_first_regex(.data$rowname,
        pattern = "^S1>S2\\.|^S2>S1\\.", replacement = ""
      ),
      "exp_mle" := exp(.data$mle) %>%
        round(2) %>%
        format(nsmall = 2) %>%
        stringi::stri_trim_both(),
      "exp_lcl" := exp(.data$lcl) %>%
        round(2) %>%
        format(nsmall = 2) %>%
        stringi::stri_trim_both(),
      "exp_ucl" := exp(.data$ucl) %>%
        round(2) %>%
        format(nsmall = 2) %>%
        stringi::stri_trim_both(),
      "OR" := paste0(
        .data$exp_mle,
        " (",
        .data$exp_lcl,
        "-",
        .data$exp_ucl,
        ")"
      )
    ) %>%
    dplyr::select("Transition", "Parameter", "OR")

  # Return
  out
}

#' Plot probabilities of observed variables by state
#'
#' Creates a plot with one pointrange for each variable and state. In the
#' project context, states are "Asthma" and "No Asthma"
#'
#' @param hmm A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' x <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 2)
#' plot_hmm_obs(x$hmm)
plot_hmm_obs <- function(hmm) {
  # Check input
  checkmate::assert_r6(hmm, "HMM", null.ok = FALSE)
  rlang::try_fetch(hmm$out(),
    error = function(cnd) {
      cli::cli_abort(c("Error!", cnd$message))
    }
  )

  # Create plot data
  df <- create_df_plot_hmm_obs(hmm)

  # Plot
  ggplot2::ggplot(mapping = ggplot2::aes(
    y = .data$rowname, x = .data$mle, xmin = .data$lcl,
    xmax = .data$ucl, color = .data$state
  )) +
    ggplot2::geom_pointrange(
      data = df$obse1,
      position = ggplot2::position_nudge(y = 0.2)
    ) +
    ggplot2::geom_pointrange(
      data = df$obse2,
      position = ggplot2::position_nudge(y = -0.2)
    ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = df$lines),
      linetype = "dashed"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      color = "State", y = NULL,
      x = "Probability of sign / symptom"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::xlim(0, 1)
}

#' Plot predictions of transitions probabilities
#'
#' @param hmm A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#' @param var A character string of length 1 containing the name of the variable
#' for which a comparison is shown inside the same plot
#' @param facet A formula passed to `facet_wrap()` or `facet_grid()`
#' @param fix A named list of length 1 vectors indicating which variables
#' (names of list elements) should be fixed to which values (vectors in list
#' elements) when creating the new data to pass to `HMM$predict()`, based on
#' which the plots are created. Variables that are not in `var` or `facet` need
#' to be fixed.
#' @param n_post A single integerish value indicating how many posterior samples
#' are drawn by `HMM$predict()`
#' @param diag_mode TRUE or FALSE (default) indicating if diagnostic mode is
#' activated
#'
#' @returns A list of two ggplot objects, `rem` and `rel`, holding the plots for
#' the effect on the probability of remission and relapse, respectively. If
#' `diag_mode = TRUE`, the list additionally contains elements `newdata`,
#' containing the element of the same name passed to `hmmTMB::HMM$predict()`,
#' `age_sex`, containing the variable names of the age and sex variables, `var`,
#' containing the name of the variable, for which the comparison of its
#' categories is included within each subplot, `facet_vars`, containing the
#' variable names used for creating subplots, and `fix_vars`, including the
#' names of variables, for which only a single value is included in `newdata`.
#'
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' x <- run_hmm(df,
#'   obsvars = c("obs1", "obs2"), nrep = 1,
#'   formula = ~ age_pred * female + vab1
#' )
#' plot_hmm_predict(x$hmm, var = "vab1", facet = ~female, n_post = 1e2)
plot_hmm_predict <- function(hmm, var = NULL, facet = NULL, fix = NULL,
                             n_post = 1e3, diag_mode = FALSE) {
  # Check packages
  rlang::check_installed(c(
    "checkmate", "cli", "purrr", "magrittr", "dplyr", "tidyselect",
    "constructive", "hmmTMB"
  ))

  # Check input
  checkmate::assert_r6(hmm, "HMM", null.ok = FALSE)
  checkmate::assert_string(var, min.chars = 1, na.ok = FALSE, null.ok = TRUE)
  checkmate::assert_formula(facet, null.ok = TRUE)
  checkmate::assert_list(fix,
    types = c("character", "numeric"), any.missing = FALSE, min.len = 1,
    null.ok = TRUE, names = "unique"
  )
  if (!is.null(fix)) {
    for (i in seq_along(fix)) {
      if (length(fix[[i]]) != 1) {
        cli::cli_abort("All elements in `fix` must be of length 1!",
          class = "plot_hmm_predict_01"
        )
      }
    }
  }
  checkmate::assert_integerish(n_post,
    lower = 1, any.missing = FALSE,
    all.missing = FALSE, len = 1, null.ok = FALSE
  )
  checkmate::assert_flag(diag_mode, na.ok = FALSE, null.ok = FALSE)

  rlang::try_fetch(hmm$out(),
    error = function(cnd) {
      cli::cli_abort(c("Error!", cnd$message))
    }
  )

  # Get variables included in tpm formulas
  tpm_form_vars <- hmm$hid()$formulas() %>%
    purrr::map(supfuns::extract.symbols.from.ast) %>%
    unlist() %>%
    unique() %>%
    magrittr::extract(
      magrittr::is_in(., c("~", "+", "*", ":")) %>%
        magrittr::not()
    )

  # Extract data of covariates
  df <- hmm$obs()$data() %>%
    dplyr::select(tidyselect::any_of(tpm_form_vars))

  # Check variables
  age_sex <- c("age_pred", "female")
  non_age_sex_vars <- colnames(df) %>%
    magrittr::extract(magrittr::is_in(., age_sex) %>% magrittr::not())


  if (!is.null(var)) {
    if (!(var %in% colnames(df))) {
      cli::cli_abort("Variable `var` was not used as predictor!",
        class = "plot_hmm_predict_02"
      )
    }
  } else {
    if (length(non_age_sex_vars) == 1) {
      var <- non_age_sex_vars
    } else {
      cli::cli_abort("Please specify in argument `var` the main variable to be
                     plotted!",
        class = "plot_hmm_predict_03"
      )
    }
  }

  if (!is.null(facet)) {
    facet_vars <- facet %>%
      supfuns::extract.symbols.from.ast() %>%
      unlist() %>%
      unique() %>%
      magrittr::extract(
        magrittr::is_in(., c("~", "+", "*", ":")) %>%
          magrittr::not()
      )
    if (facet_vars %>%
      magrittr::is_in(colnames(df)) %>%
      all() %>%
      magrittr::not()) {
      cli::cli_abort("Variables used in `facet` must be used as transition
                     predictors!",
        class = "plot_hmm_predict_04"
      )
    }
  } else {
    facet_vars <- NULL
  }

  if (!is.null(fix)) {
    fix_vars <- names(fix)
    if (fix_vars %>%
      magrittr::is_in(colnames(df)) %>%
      all() %>%
      magrittr::not()) {
      cli::cli_abort("Variables used in `fix` must be used as transition
                     predictors!",
        class = "plot_hmm_predict_05"
      )
    }
  } else {
    fix_vars <- NULL
  }

  if (age_sex %>% magrittr::is_in(var) %>% any()) {
    cli::cli_abort("Variables `age_pred` and `female` cannot be specified as
                   `var`!",
      class = "plot_hmm_predict_06"
    )
  }

  if (age_sex %>% magrittr::is_in(fix_vars) %>% any()) {
    cli::cli_abort("Variables `age_pred` and `female` cannot be specified in
                   `fix`!",
      class = "plot_hmm_predict_07"
    )
  }

  if ("age_pred" %>% magrittr::is_in(facet_vars) %>% any()) {
    cli::cli_abort("Variable `age_pred` cannot be specified in `facet`!",
      class = "plot_hmm_predict_08"
    )
  }

  if (facet_vars %>% magrittr::is_in(var) %>% any()) {
    cli::cli_abort("Variables in `facet` cannot be specified as `var`!",
      class = "plot_hmm_predict_09"
    )
  }

  if (fix_vars %>% magrittr::is_in(var) %>% any()) {
    cli::cli_abort("Variables in `fix` cannot be specified as `var`!",
      class = "plot_hmm_predict_10"
    )
  }

  if (facet_vars %>% magrittr::is_in(fix_vars) %>% any()) {
    cli::cli_abort("Variables in `facet` cannot be specified in `fix`!",
      class = "plot_hmm_predict_11"
    )
  }

  # Create facet
  if (is.null(facet)) {
    if (length(non_age_sex_vars) <= 1) {
      facet <- stats::formula(~female)
      facet_type <- "wrap"
      facet_vars <- "female"
    } else {
      cli::cli_abort("Please define argument `facet`!",
        class = "plot_hmm_predict_12"
      )
    }
  } else {
    if (length(facet) == 2) {
      facet_type <- "wrap"
    } else if (length(facet) == 3) {
      facet_type <- "grid"
    } else {
      cli::cli_abort("Error in argument `facet`!")
    }
  }

  # Create newdata
  newdata_vars <- c(age_sex, var, facet_vars, fix_vars) %>% unique()

  if (colnames(df) %>%
    magrittr::is_in(newdata_vars) %>%
    magrittr::not() %>%
    any()) {
    cli::cli_abort("All predictors of the transition probability matrix (apart
                   from `age_pred`) must be used in arguments `var`, `facet` or
                   `fix`!", class = "plot_hmm_predict_13")
  }

  eg <- vector(mode = "list", length = length(newdata_vars)) %>%
    magrittr::set_names(newdata_vars)

  for (i in seq_along(df)) {
    if (newdata_vars[i] == "age_pred") {
      eg[["age_pred"]] <- rlang::expr(0:12)
    } else if (newdata_vars[i] %in% fix_vars) {
      eg[[newdata_vars[i]]] <- rlang::expr(!!fix[[newdata_vars[i]]])
    } else {
      x <- df[[newdata_vars[i]]] %>%
        unique() %>%
        sort() %>%
        constructive::construct() %>%
        magrittr::extract2("code") %>%
        as.character() %>%
        rlang::parse_expr()

      eg[[newdata_vars[i]]] <- rlang::expr(!!x)
    }
  }

  newdata <- rlang::inject(expand.grid(!!!eg))

  # Predict
  pred <- hmm$predict("tpm", newdata = newdata, n_post = n_post)

  newdata$rem_est <- NA_real_
  newdata$rem_lci <- NA_real_
  newdata$rem_uci <- NA_real_
  newdata$rel_est <- NA_real_
  newdata$rel_lci <- NA_real_
  newdata$rel_uci <- NA_real_

  for (i in seq_len(nrow(newdata))) {
    newdata$rem_est[i] <- pred$mle[2, 1, i]
    newdata$rem_lci[i] <- pred$lcl[2, 1, i]
    newdata$rem_uci[i] <- pred$ucl[2, 1, i]

    newdata$rel_est[i] <- pred$mle[1, 2, i]
    newdata$rel_lci[i] <- pred$lcl[1, 2, i]
    newdata$rel_uci[i] <- pred$ucl[1, 2, i]
  }
  rm(i)

  rem <- newdata %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$age_pred, y = .data$rem_est)) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = .data$rem_lci, ymax = .data$rem_uci,
      fill = .data[[var]]
    ), alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(color = .data[[var]])) +
    ggplot2::geom_point(ggplot2::aes(color = .data[[var]])) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12),
      labels = c("12", "18", "24")
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::scale_fill_brewer(palette = "Accent") +
    ggplot2::labs(y = "Probability of remission", x = "Age (years)")

  if (facet_type == "wrap") {
    rem <- rem +
      ggplot2::facet_wrap(facet)
  }
  if (facet_type == "grid") {
    rem <- rem +
      ggplot2::facet_grid(facet)
  }

  rel <- newdata %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$age_pred, y = .data$rel_est)) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = .data$rel_lci, ymax = .data$rel_uci,
      fill = .data[[var]]
    ), alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(color = .data[[var]])) +
    ggplot2::geom_point(ggplot2::aes(color = .data[[var]])) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12),
      labels = c("12", "18", "24")
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::scale_fill_brewer(palette = "Accent") +
    ggplot2::labs(y = "Probability of relapse", x = "Age (years)")

  if (facet_type == "wrap") {
    rel <- rel +
      ggplot2::facet_wrap(facet)
  }
  if (facet_type == "grid") {
    rel <- rel +
      ggplot2::facet_grid(facet)
  }

  if (diag_mode) {
    list(
      rem = rem, rel = rel, newdata = newdata, age_sex = age_sex, var = var,
      facet_vars = facet_vars, fix_vars = fix_vars
    )
  } else {
    list(rem = rem, rel = rel)
  }
}

#' Creating the data.frame for `plot_hmm_obs()`
#'
#' @param hmm The object passed to argument `hmm` in function `plot_hmm_obs()`
#'
#' @returns A list of length 4 with elements `obse`,
#'
#' @noRd
create_df_plot_hmm_obs <- function(hmm) {
  obse <- hmm$confint()$coeff_fe$obs %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(
      "se" := NULL,
      "mle" := stats::plogis(.data$mle),
      "lcl" := stats::plogis(.data$lcl),
      "ucl" := stats::plogis(.data$ucl),
      "rowname" := stringi::stri_replace_all_fixed(.data$rowname,
        pattern = ".(Intercept)",
        replacement = ""
      ),
      "rowname" := stringi::stri_replace_all_fixed(.data$rowname,
        pattern = ".p2",
        replacement = ""
      ),
      "state" := stringi::stri_extract_all_regex(.data$rowname, ".$") %>%
        as.character(),
      "rowname" := stringi::stri_replace_all_regex(.data$rowname,
        pattern = "\\.state.$",
        replacement = ""
      ),
      "state" := factor(.data$state,
        levels = c("1", "2"),
        labels = c("No Asthma", "Asthma")
      ),
      "rowname" := factor(.data$rowname) %>% forcats::fct_rev()
    )

  obse1 <- obse[obse$state == "No Asthma", ]
  obse2 <- obse[obse$state == "Asthma", ]
  lines <- seq(
    from = 1.5,
    to = obse$rowname %>%
      nlevels() %>%
      magrittr::subtract(0.5),
    by = 1
  )

  list(obse = obse, obse1 = obse1, obse2 = obse2, lines = lines)
}

#' Create and fit a Hidden Markov Model
#'
#' Creates a Hidden Markov Model with `hmmTMB` and fits it with different sets
#' of random starting values, reporting identification and the best model.
#'
#' @param data A data.frame which is passed to argument `data` of the
#' `hmmTMB::Observation` object
#' @param obsvars A character vector including the names of the variables
#' included in the observation model
#' @param initial_state A character string, either "shared" (default) or
#' "stationary" containing the value that is passed to argument `initial_state`
#' of the `hmmTMB::MarkovChain` object
#' @param formula A formula object describing the linear model of the transition
#' probabilities, which is passed to argument `formula` of the
#' `hmmTMB::MarkovChain` object. Default is `NULL` corresponding to the
#' intercept model without any predictors.
#' @param nrep A single integerish value indicating the number of repetitions
#' of model fitting with different starting values. Default (and recommended
#' value) is 100.
#' @param diag_mode TRUE or FALSE (default) indicating if diagnostic mode is
#' activated
#'
#' @returns With `diag_mode = FALSE`, a list of length 2 with elements `hmm`
#' containing the best of the `nrep` models, and `ident`, a number between 0 and
#' 1, indicating what percentage of the `nrep` models has the same likelihood as
#' the best model. `ident` of 1 indicates perfect identification because all
#' models led to the same maximum likelihood solution, indicating that the
#' global maximum has been found. In latent class analysis, we are reasonable
#' sure to have found the global maximum likelihood solution when 50-60%
#' (`ident` equals 0.5-0.6) of models, i.e., starting values, lead to the same
#' likelihood as the best model. With `diag_mode = TRUE`, the list additionally
#' contains the following elements: `diag_all_models`, a list of all models
#' before selecting the best one, `diag_hid_draws`, a list of the drawn starting
#' values for the transition parameters, `diag_hid_updated`, a list of the `HMM`
#' object's values for the transition parameters after updating the starting
#' values, `diag_obs_draws`, a list of the drawn starting values for symptom
#' probabilities by state, `diag_obs_updated`, a list of the `HMM` object's
#' values for the symptom probabilities by state after updating the starting
#' values, `diag_unfitted_hid`, the default starting values for the transition
#' parameters, and `diag_unfitted_obs`, the default starting values for the
#' symptom probabilities by state.
#'
#' @export
#'
#' @examples
#' df <- create_hmm_data()
#' hmm <- run_hmm(df, obsvars = c("obs1", "obs2"), nrep = 1)
run_hmm <- function(data, obsvars, initial_state = c("shared", "stationary"),
                    formula = NULL, nrep = 100, diag_mode = FALSE) {
  # Check input
  checkmate::assert_data_frame(data, null.ok = FALSE)
  initial_state <- rlang::arg_match(initial_state)
  checkmate::assert_formula(formula, null.ok = TRUE)
  checkmate::assert_character(obsvars,
    min.chars = 1, any.missing = FALSE,
    min.len = 1, null.ok = FALSE, unique = TRUE
  )
  checkmate::assert_subset(obsvars, choices = colnames(data))
  checkmate::assert_integerish(nrep,
    lower = 1, any.missing = FALSE,
    all.missing = FALSE, len = 1, null.ok = FALSE
  )
  checkmate::assert_flag(diag_mode, na.ok = FALSE, null.ok = FALSE)

  formula_vars <- formula %>%
    supfuns::extract.symbols.from.ast() %>%
    unlist() %>%
    unique() %>%
    magrittr::extract(
      magrittr::is_in(., c("~", "+", "*", ":")) %>%
        magrittr::not()
    )
  checkmate::assert_subset(formula_vars, choices = colnames(data))

  # Hidden state process
  hid <- hmmTMB::MarkovChain$new(
    data = data,
    n_states = 2,
    initial_state = initial_state,
    formula = formula
  )

  # Observation model
  dists <- rep(list("cat"), length(obsvars)) %>%
    magrittr::set_names(obsvars)

  par <- rep(list(list(p2 = c(0.5, 0.5))), length(obsvars)) %>%
    magrittr::set_names(obsvars)

  obs <- hmmTMB::Observation$new(
    data = data,
    n_states = 2,
    dists = dists,
    par = par
  )

  # Hidden Markov model
  hmm <- hmmTMB::HMM$new(obs = obs, hid = hid)

  # Diagnostic mode
  if (diag_mode) {
    diag_unfitted_hid <- hmm$coeff_fe()$hid
    diag_unfitted_obs <- hmm$coeff_fe()$obs
  }

  # Get number of transition parameters
  n_hid_par <- hmm$coeff_fe()$hid %>% nrow()

  # Model fitting
  if (nrep == 1) {
    ## Draw random starting values
    a <- stats::runif(n = 2, min = -4.59, max = -0.84)
    b <- stats::runif(n = (n_hid_par - 2), min = -1.1, max = 1.1)
    o <- stats::runif(n = (length(obsvars) * 2), min = 0.001, max = 0.999)

    ## Update parameters in hmm with starting values
    if (!is.null(formula)) {
      hid_new <- c(
        a[1], b[1:((n_hid_par - 2) / 2)],
        a[2], b[((n_hid_par - 2) / 2 + 1):(n_hid_par - 2)]
      )
    } else {
      hid_new <- a
    }
    hid$update_coeff_fe(hid_new)

    newpar <- par
    for (j in seq_along(newpar)) {
      newpar[[j]][[1]][1] <- o[(j - 1) * 2 + 1]
      newpar[[j]][[1]][2] <- o[(j - 1) * 2 + 2]
    }
    obs$update_par(par = newpar)

    ## Diagnostic mode
    if (diag_mode) {
      diag_hid_draws <- hid_new
      diag_hid_updated <- hmm$coeff_fe()$hid
      diag_obs_draws <- o
      diag_obs_updated <- hmm$coeff_fe()$obs
    }

    ## Fit
    hmm$fit(silent = TRUE)

    ## Diagnostic mode
    if (diag_mode) {
      diag_all_models <- NULL
    }

    ## Output
    best_model <- hmm
    ident <- NA
  } else {
    ## Start progressr
    p_bar <- progressr::progressor(steps = nrep)

    ## Model
    foreach_result <- foreach::foreach(
      i = 1:nrep,
      a = iterators::irunif(n = 2, min = -4.59, max = -0.84, count = nrep),
      b = iterators::irunif(
        n = (n_hid_par - 2), min = -1.1, max = 1.1,
        count = nrep
      ),
      o = iterators::irunif(
        n = (length(obsvars) * 2), min = 0.001, max = 0.999,
        count = nrep
      ),
      .options.future = list(packages = c("hmmTMB", "magrittr"))
    ) %dofuture% {
      ## Update progressr bar
      p_bar()

      ## To please 'R CMD check'
      a <- a
      b <- b
      o <- o
      i <- i

      ## Update parameters in hmm with starting values
      if (!is.null(formula)) {
        hid_new <- c(
          a[1], b[1:((n_hid_par - 2) / 2)],
          a[2], b[((n_hid_par - 2) / 2 + 1):(n_hid_par - 2)]
        )
      } else {
        hid_new <- a
      }
      hid$update_coeff_fe(hid_new)

      newpar <- par
      for (j in seq_along(newpar)) {
        newpar[[j]][[1]][1] <- o[(j - 1) * 2 + 1]
        newpar[[j]][[1]][2] <- o[(j - 1) * 2 + 2]
      }
      obs$update_par(par = newpar)

      ## Diagnostic mode
      if (diag_mode) {
        diag_hid_draws <- hid_new
        diag_hid_updated <- hmm$coeff_fe()$hid
        diag_obs_draws <- o
        diag_obs_updated <- hmm$coeff_fe()$obs
      }

      ## Fit
      hmm$fit(silent = TRUE)

      # Return
      if (diag_mode) {
        list(
          model = hmm,
          diag_hid_draws = diag_hid_draws,
          diag_hid_updated = diag_hid_updated,
          diag_obs_draws = diag_obs_draws,
          diag_obs_updated = diag_obs_updated
        )
      } else {
        list(model = hmm)
      }
    }

    models <- foreach_result %>% purrr::map(magrittr::extract("model"))

    ## Diagnostic mode
    if (diag_mode) {
      diag_all_models <- foreach_result %>%
        purrr::map(magrittr::extract("model"))
      diag_hid_draws <- foreach_result %>%
        purrr::map(magrittr::extract("diag_hid_draws"))
      diag_hid_updated <- foreach_result %>%
        purrr::map(magrittr::extract("diag_hid_updated"))
      diag_obs_draws <- foreach_result %>%
        purrr::map(magrittr::extract("diag_obs_draws"))
      diag_obs_updated <- foreach_result %>%
        purrr::map(magrittr::extract("diag_obs_updated"))
    }

    ## Output
    best_model <- models %>% purrr::reduce(best_hmm)
    ident <- models %>%
      purrr::map_dbl(\(x) x$llk()) %>%
      round(3) %>%
      magrittr::equals(min(.)) %>%
      mean()
  }

  # Return
  if (diag_mode) {
    list(
      hmm = best_model, ident = ident, diag_all_models = diag_all_models,
      diag_hid_draws = diag_hid_draws, diag_hid_updated = diag_hid_updated,
      diag_obs_draws = diag_obs_draws, diag_obs_updated = diag_obs_updated,
      diag_unfitted_hid = diag_unfitted_hid,
      diag_unfitted_obs = diag_unfitted_obs
    )
  } else {
    list(hmm = best_model, ident = ident)
  }
}

#' Select HMM with better likelihood
#'
#' Select HMM with lower negative log-likelihood
#'
#' @param hmm1 A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#' @param hmm2 A `hmmTMB::HMM` object, which has already been fitted with
#' `HMM$fit()`
#'
#' @returns One of `hmm1` and `hmm2`
#'
#' @noRd
best_hmm <- function(hmm1, hmm2) {
  if (hmm1$llk() <= hmm2$llk()) {
    hmm1
  } else {
    hmm2
  }
}

# trans_desc <- function(hmm, vars, nsamp = 1, diag_mode = FALSE) {
#   # Check input
#   checkmate::assert_r6(hmm, "HMM", null.ok = FALSE)
#   checkmate::assert_integerish(nsamp,
#                                lower = 1, any.missing = FALSE,
#                                all.missing = FALSE, len = 1, null.ok = FALSE
#   )
#   checkmate::assert_flag(diag_mode, na.ok = FALSE, null.ok = FALSE)
#
#   rlang::try_fetch(hmm$out(),
#                    error = function(cnd) {
#                      cli::cli_abort(c("Error!", cnd$message))
#                    }
#   )
#
#   # Extract data
#   df <- hmm$obs()$data()
#
#   rlang::try_fetch(checkmate::assert_subset(c("ID", "time"), colnames(df)),
#                    error = function(cnd) {
#                      cli::cli_abort("Dataset needs variables `ID` and `time`!")
#                    }
#   )
#
#   # Prepare data
#   df %<>% dplyr::select(tidyselect::all_of(c("ID", "time", vars)))
#
#   if (nsamp == 1) {
#     df %<>% cbind(hmm$viterbi() %>%
#                     as.data.frame() %>%
#                     magrittr::set_colnames("s1"))
#   } else {
#     df %<>% cbind(hmm$sample_states(nsamp = nsamp) %>%
#                     as.data.frame() %>%
#                     magrittr::set_colnames(paste0("s", 1:nsamp)))
#   }
#
#   # Derive transitions
#   uids <- df$ID %>% levels()
#   out <- vector(mode = "list", length = nsamp)
#
#   for (i in 1:nsamp) {
#     out[[i]] <- vector(mode = "list", length = length(uids))
#
#     temp <- df %>%
#       dplyr::select(tidyselect::all_of(c("ID", "time", paste0("s", i)))) %>%
#       magrittr::set_colnames(c("ID", "time", "s"))
#
#     for (j in seq_along(uids)) {
#       temp_id <- temp %>%
#         dplyr::filter(.data$ID == uids[j]) %>%
#         dplyr::arrange(.data$time)
#
#       temp_id$t <- NA_character_
#
#       for (k in 1:(nrow(temp_id) - 1)) {
#         temp_id$t[k] <- paste0(
#           temp_id$s[k],
#           "->",
#           temp_id$s[k + 1]
#         )
#       }
#
#       out[[i]][[j]] <- temp_id
#     }
#
#     out[[i]] %<>% purrr::reduce(rbind)
#   }
#
#   out %<>% purrr::imap(\(x, idx) {
#     if (identical(colnames(x), c("ID", "time", "s", "t"))) {
#       magrittr::set_colnames(
#         x,
#         c("ID", "time", paste0("s", idx), paste0("t", idx))
#       )
#     }
#   })
#
#   out %<>% purrr::reduce(dplyr::left_join, by = c("ID", "time"))
#
#   df %<>%
#     dplyr::select(tidyselect::all_of(c("ID", "time", vars))) %>%
#     dplyr::left_join(out, by = c("ID", "time"))
#
#   # Return
#
#   df
# }
